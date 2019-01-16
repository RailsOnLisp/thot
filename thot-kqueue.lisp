;;
;;  Thot - http web server
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com> 0614550127
;;

(in-package :thot)

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

(defgeneric agent-kqueue-events (agent))
(defgeneric agent-error (kqueue agent))
(defgeneric agent-in (kqueue agent))
(defgeneric agent-out (kqueue agent))

(define-condition agent-error (error)
  ((agent :initarg :agent
          :reader agent-error-agent
          :type agent)))

;;  Adding an agent

(defmacro get-agent (kqueue fd)
  `(gethash ,fd (kqueue-agents ,kqueue)))

(defun remove-agent (kqueue fd)
  (declare (type kqueue-infos kqueue))
  (remhash fd (kqueue-agents kqueue)))

(defun kqueue-add (kqueue agent)
  (declare (type kqueue-infos kqueue))
  (let ((fd (agent-fd agent)))
    (set-nonblocking fd)
    (setf (get-agent kqueue fd) agent)
    (kqueue:add (kqueue-fd kqueue) fd
               (agent-kqueue-events agent)
               :data-fd fd)))

(defun kqueue-del (kqueue agent)
  (declare (type kqueue-infos kqueue))
  (let ((fd (agent-fd agent)))
    (kqueue:del (kqueue-fd kqueue) fd)
    (socket:shutdown fd t t)
    (unistd:close fd)
    (remove-agent kqueue fd)))

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

(defmethod agent-kqueue-events ((worker worker))
  (logior kqueue:+in+ kqueue:+out+ kqueue:+err+))

(define-condition worker-error (agent-error)
  ())

(defmethod agent-error ((kqueue kqueue-infos) (worker worker))
  (error 'worker-error :agent worker))

(defmethod agent-in ((kqueue kqueue-infos) (worker worker))
  (let ((reader-cont (worker-reader-cont worker)))
    (when reader-cont
      (let ((result (handler-case (funcall reader-cont)
                      (warning (x) (format t "~A~%" x) :eof))))
        (cond ((eq :eof result) (kqueue-del kqueue worker))
              ((eq nil result) (setf (worker-reader-cont worker) nil))
              ((eq :keep-alive result) (setf (worker-keep-alive worker) t
                                             (worker-reader-cont worker) nil)
               :keep-alive)
              ((functionp result) (setf (worker-reader-cont worker) result))
              (t (error "worker input error ~S" worker)))))))

(defmethod agent-out ((kqueue kqueue-infos) (worker worker))
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
                  (agent-in kqueue worker))
                 (t
                  (kqueue-del kqueue worker))))
          (t
           (case (stream-flush-output stream)
             ((nil) nil)
             ((:eof) (kqueue-del kqueue worker))
             ((:non-blocking) :non-blocking)
             (otherwise (error 'stream-output-error :stream stream)))))))

;;  Acceptor agent

(defclass acceptor (agent)
  ())

(defmethod agent-kqueue-events ((agent acceptor))
  (logior kqueue:+in+ kqueue:+err+))

(define-condition acceptor-error (agent-error)
  ())

(defmethod agent-error ((kqueue kqueue-infos) (acceptor acceptor))
  (error 'acceptor-error :agent acceptor))

(defun make-worker (fd addr)
  (let* ((request-stream (babel-input-stream (unistd-input-stream fd)))
         (reply-stream (babel-output-stream
                        (multi-buffered-output-stream
                         (unistd-output-stream fd))))
         (request (make-instance 'request :stream request-stream))
         (reply (make-instance 'reply :stream reply-stream
                               :remote-addr (socket:sockaddr-to-string
                                             addr)))
         (reader-cont (request-reader request reply)))
    (make-instance 'worker
                   :addr addr
                   :fd fd
                   :reader-cont reader-cont
                   :request request
                   :reply reply)))

(defmethod agent-in ((kqueue kqueue-infos) (acceptor acceptor))
  (multiple-value-bind (fd addr) (socket:accept (agent-fd acceptor))
    (unless (eq :non-blocking fd)
      (kqueue-add kqueue (make-worker fd addr)))))

(defclass control (agent)
  ())

(defmethod agent-kqueue-events ((agent control))
  kqueue:+in+)

(defmethod agent-in ((kqueue kqueue-infos) (agent control))
  (setq *stop* t))

;;  Thread event loop

(defun acceptor-loop-kqueue (listenfd &optional pipe)
  (declare (type unistd:file-descriptor listenfd))
  (kqueue:with-kqueue (kqueue-fd)
    (let ((kqueue (make-instance 'kqueue-infos :fd kqueue-fd)))
      (kqueue-add kqueue (make-instance 'acceptor :fd listenfd))
      (when pipe
        (kqueue-add kqueue (make-instance 'control :fd pipe)))
      (loop
         (when *stop*
           (return))
         (kqueue:wait (events fd kqueue-fd)
                     (let ((agent (get-agent kqueue fd)))
                       (unless agent (error "bad kqueue fd ~S" fd))
                       (cond ((not (= 0 (logand kqueue:+err+ events)))
                              (agent-error kqueue agent))
                             ((not (= 0 (logand kqueue:+in+ events)))
                              (agent-in kqueue agent))
                             ((not (= 0 (logand kqueue:+out+ events)))
                              (agent-out kqueue agent)))))))))

(defun maybe-configure-kqueue ()
  (when (cffi:foreign-symbol-pointer "kqueue")
    (setf *acceptor-loop* #'acceptor-loop-kqueue)))

(eval-when (:load-toplevel :execute)
  (maybe-configure-kqueue))

;;(untrace )
