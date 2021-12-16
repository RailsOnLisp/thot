;;
;;  Thot - http web server
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com> 0614550127
;;

(in-package :thot)

(use-package :kqueue)

;;  kqueue infos

(defclass kqueue-infos ()
  ((fd :initarg :fd
       :reader kqueue-fd
       :type (unsigned-byte 31))
   (agents :initform (make-hash-table)
           :reader kqueue-agents
           :type hash-table)))

;;  Generic kqueue agent class

(defclass agent ()
  ((fd :initarg :fd
       :reader agent-fd
       :type (unsigned-byte 31))))

(defgeneric agent-kqueue-filters (agent))
(defgeneric agent-error (kqueue agent))
(defgeneric agent-in (kqueue agent))
(defgeneric agent-out (kqueue agent))

(define-condition agent-error (error)
  ((agent :initarg :agent
          :reader agent-error-agent
          :type agent)))

;;  Adding an agent

(defmacro get-agent (kq fd)
  `(gethash ,fd (kqueue-agents ,kq)))

(defun remove-agent (kq fd)
  (declare (type kqueue-infos kq))
  (remhash fd (kqueue-agents kq)))

(defun kqueue-add (kq agent)
  (declare (type kqueue-infos kq))
  (let* ((fd (agent-fd agent))
         (filters (agent-kqueue-filters agent))
         (filters-length (length filters)))
    (set-nonblocking fd)
    (setf (get-agent kq fd) agent)
    (cffi:with-foreign-object (change '(:struct kevent) filters-length)
      (dotimes (i filters-length)
        (let ((kev (mem-aptr change '(:struct kevent) i)))
          (setf (kevent-ident kev) fd
                (kevent-filter kev) (nth i filters)
                (kevent-flags kev) +ev-add+
                (kevent-fflags kev) 0
                (kevent-data kev) 0
                (kevent-udata kev) (null-pointer))))
      (kqueue:kevent (kqueue-fd kq) :changes change
                     :n-changes filters-length))))

(defun kqueue-del (kq agent)
  (declare (type kqueue-infos kq))
  (let* ((fd (agent-fd agent))
         (filters (agent-kqueue-filters agent))
         (filters-length (length filters)))
    (cffi:with-foreign-object (change '(:struct kevent) filters-length)
      (dotimes (i filters-length)
        (declare (type fixnum i))
        (let ((kev (mem-aptr change '(:struct kevent) i)))
          (setf (kevent-ident kev) fd
                (kevent-filter kev) (nth i filters)
                (kevent-flags kev) +ev-delete+
                (kevent-fflags kev) 0
                (kevent-data kev) 0
                (kevent-udata kev) (cffi:null-pointer))))
      (kqueue:kevent (kqueue-fd kq) :changes change
                     :n-changes filters-length))
    (socket:shutdown fd t t)
    (unistd:close fd)
    (remove-agent kq fd)))

;;  Worker agent

(defclass worker (agent)
  ((addr :initarg :addr
         :reader worker-addr)
   (keep-alive :initform nil
               :accessor worker-keep-alive
               :type boolean)
   (reader-cont :initarg :reader-cont
                :accessor worker-reader-cont
                :type (or null function))
   (reply :initarg :reply
          :accessor worker-reply
          :type reply)
   (request :initarg :request
            :accessor worker-request
            :type request)))

(defmethod agent-kqueue-filters ((worker worker))
  (list +evfilt-read+ +evfilt-write+))

(defmethod agent-in ((kq kqueue-infos) (worker worker))
  (let ((reader-cont (worker-reader-cont worker)))
    (when reader-cont
      (let ((result (handler-case (funcall reader-cont)
                      (warning (x) (format t "~A~%" x) :eof))))
        (cond ((eq :eof result) (kqueue-del kq worker))
              ((eq nil result) (setf (worker-reader-cont worker) nil))
              ((eq :keep-alive result) (setf (worker-keep-alive worker) t
                                             (worker-reader-cont worker) nil)
               :keep-alive)
              ((functionp result) (setf (worker-reader-cont worker) result))
              (t (error "worker input error ~S" worker)))))))

(defmethod agent-out ((kq kqueue-infos) (worker worker))
  (let* ((request (worker-request worker))
         (reply (worker-reply worker))
         (babel-stream (reply-stream reply))
         (stream (stream-underlying-stream babel-stream)))
    (cond ((and (null (worker-reader-cont worker))
                (= 0 (the integer (stream-output-length stream))))
           (cond ((worker-keep-alive worker)
                  ;; read request body
                  (setf (worker-reader-cont worker)
                        (request-reader (reset-request request)
                                        (reset-reply reply)))
                  (agent-in kq worker))
                 (t
                  (kqueue-del kq worker))))
          (t
           (case (stream-flush-output stream)
             ((nil) nil)
             ((:eof) (kqueue-del kq worker))
             ((:non-blocking) :non-blocking)
             (otherwise (error 'stream-output-error :stream stream)))))))

;;  Acceptor agent

(defclass acceptor (agent)
  ())

(defmethod agent-kqueue-filters ((agent acceptor))
  (list +evfilt-read+))

(defun make-worker (fd addr)
  (let* ((request-stream (babel-input-stream (unistd-input-stream fd)))
         (reply-stream (babel-output-stream
                        (multi-buffered-output-stream
                         (unistd-output-stream fd))))
         (request (make-instance 'request :stream request-stream
                                 :remote-addr (socket:sockaddr-to-string
                                               addr)))
         (reply (make-instance 'reply :stream reply-stream))
         (reader-cont (request-reader request reply)))
    (make-instance 'worker
                   :addr addr
                   :fd fd
                   :reader-cont reader-cont
                   :request request
                   :reply reply)))

(defmethod agent-in ((kq kqueue-infos) (acceptor acceptor))
  (multiple-value-bind (fd addr) (socket:accept (agent-fd acceptor))
    (unless (eq :non-blocking fd)
      (kqueue-add kq (make-worker fd addr)))))

(defclass control (agent)
  ())

(defmethod agent-kqueue-filters ((agent control))
  (list +evfilt-read+))

(defmethod agent-in ((kq kqueue-infos) (agent control))
  (setq *stop* t))

;;  Thread event loop

(defun acceptor-loop-kqueue (listenfd &optional pipe)
  (declare (type unistd:file-descriptor listenfd))
  (kqueue:with-kqueue (kq-fd)
    (let ((kq (make-instance 'kqueue-infos :fd kq-fd)))
      (kqueue-add kq (make-instance 'acceptor :fd listenfd))
      (when pipe
        (kqueue-add kq (make-instance 'control :fd pipe)))
      (loop
         (when *stop*
           (return))
         (with-foreign-objects ((events '(:struct kqueue:kevent) 10000)
                                (timeout '(:struct kqueue:timespec)))
           (kqueue:seconds-to-timespec timeout 10)
           (let ((n-events (kqueue:kevent kq-fd :events events
                                          :n-events 10000
                                          :timeout timeout)))
             (declare (type fixnum n-events))
             (dotimes (i n-events)
               (let* ((event (mem-aptr events '(:struct kevent) i))
                      (filter (kqueue:kevent-filter event))
                      (fd (kqueue:kevent-ident event))
                      (agent (get-agent kq fd)))
                 (when agent
                   (cond ((= filter kqueue:+evfilt-read+)
                          (agent-in kq agent))
                         ((= filter kqueue:+evfilt-write+)
                          (agent-out kq agent))))))))))))

(defun maybe-configure-kqueue ()
  (when (cffi:foreign-symbol-pointer "kqueue")
    (setf *acceptor-loop* #'acceptor-loop-kqueue)))

(eval-when (:load-toplevel :execute)
  (maybe-configure-kqueue))

;;(untrace )
