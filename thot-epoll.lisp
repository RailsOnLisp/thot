;;
;;  Thot - http web server
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com> 0614550127
;;

(in-package :thot)

;;  epoll infos

(defclass epoll-infos ()
  ((fd :initarg :fd
       :reader epoll-fd
       :type (unsigned-byte 31))
   (agents :initform (make-hash-table)
           :reader epoll-agents
           :type hash-table)))

;;  Generic epoll agent class

(defclass agent ()
  ((fd :initarg :fd
       :reader agent-fd
       :type (unsigned-byte 31))
   (pending :initform nil
            :accessor agent-pending
            :type boolean)))

(defgeneric agent-epoll-events (agent))
(defgeneric agent-error (epoll agent))
(defgeneric agent-in (epoll agent))
(defgeneric agent-out (epoll agent))

(define-condition agent-error (error)
  ((agent :initarg :agent
          :reader agent-error-agent
          :type agent)))

;;  Adding an agent

(defmacro get-agent (epoll fd)
  `(gethash ,fd (epoll-agents ,epoll)))

(defun remove-agent (epoll fd)
  (declare (type epoll-infos epoll))
  (remhash fd (epoll-agents epoll)))

(defun epoll-add (epoll agent)
  (declare (type epoll-infos epoll))
  (let ((fd (agent-fd agent)))
    (set-nonblocking fd)
    (setf (get-agent epoll fd) agent)
    (epoll:add (epoll-fd epoll) fd
               (agent-epoll-events agent)
               :data-fd fd)))

(defun epoll-mod (epoll agent events)
  (declare (type epoll-infos epoll))
  (let ((fd (agent-fd agent)))
    (epoll:mod (epoll-fd epoll) fd events :data-fd fd)))

(defun epoll-del (epoll agent)
  (declare (type epoll-infos epoll))
  (setf (agent-pending agent) t)
  (let ((fd (agent-fd agent)))
    (epoll:del (epoll-fd epoll) fd)
    (socket:shutdown fd t t)
    (unistd:close fd)
    (remove-agent epoll fd)))

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

(defmethod agent-epoll-events ((worker worker))
  (logior epoll:+in+ epoll:+out+ epoll:+err+))

(define-condition worker-error (agent-error)
  ())

(defmethod agent-error ((epoll epoll-infos) (worker worker))
  (epoll-del epoll worker))

(defmethod agent-in ((epoll epoll-infos) (worker worker))
  (let ((reader-cont (worker-reader-cont worker)))
    (if reader-cont
        (let ((result (funcall reader-cont)))
          (cond ((null result) (setf (worker-reader-cont worker) nil))
                ((eq :keep-alive result)
                 (let ((request (worker-request worker))
                       (reply (worker-reply worker)))
                   (setf (worker-reader-cont worker)
                         (request-reader (reset-request request)
                                         (reset-reply reply)))))
                ((functionp result) (setf (worker-reader-cont worker)
                                          result))
                ((eq :eof result) (epoll-del epoll worker))
                (t (error "worker input error ~S" worker))))
        (when (= 0 (stream-output-length
                    (reply-stream (worker-reply worker))))
          (epoll-del epoll worker)))))

(defmethod agent-out ((epoll epoll-infos) (worker worker))
  (let* ((request (worker-request worker))
         (reply (worker-reply worker))
         (babel-stream (reply-stream reply))
         (stream (stream-underlying-stream babel-stream)))
    (case (stream-flush-output stream)
      ((:eof) (epoll-del epoll worker)))
    (unless (worker-reader-cont worker)
      (when (= 0 (stream-output-length stream))
        (epoll-del epoll worker)))))

;;  Acceptor agent

(defclass acceptor (agent)
  ())

(defmethod agent-epoll-events ((agent acceptor))
  (logior epoll:+in+ epoll:+err+))

(define-condition acceptor-error (agent-error)
  ())

(defmethod agent-error ((epoll epoll-infos) (acceptor acceptor))
  (error 'acceptor-error :agent acceptor))

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

(defmethod agent-in ((epoll epoll-infos) (acceptor acceptor))
  (multiple-value-bind (fd addr) (socket:accept (agent-fd acceptor))
    (unless (eq :non-blocking fd)
      (let ((worker (make-worker fd addr)))
        (epoll-add epoll worker)))))

(defclass control (agent)
  ())

(defmethod agent-epoll-events ((agent control))
  epoll:+in+)

(defmethod agent-in ((epoll epoll-infos) (agent control))
  (setq *stop* t))

;;  Thread event loop

(defun acceptor-loop-epoll (listenfd &optional pipe)
  (declare (type unistd:file-descriptor listenfd))
  (epoll:with (epoll-fd)
    (let ((epoll (make-instance 'epoll-infos :fd epoll-fd)))
      (epoll-add epoll (make-instance 'acceptor :fd listenfd))
      (when pipe
        (epoll-add epoll (make-instance 'control :fd pipe)))
      (loop
         (when *stop*
           (return))
         (epoll:wait (events fd epoll-fd 10000 -1)
                     (let ((agent (get-agent epoll fd)))
                       (unless agent (error "bad epoll fd ~S" fd))
                       (unless (= 0 (logand epoll:+err+ events))
                         (agent-error epoll agent))
                       (unless (or (agent-pending agent)
                                   (= 0 (logand epoll:+in+ events)))
                         (agent-in epoll agent))
                       (unless (or (agent-pending agent)
                                   (= 0 (logand epoll:+out+ events)))
                         (agent-out epoll agent))))))))

(defun maybe-configure-epoll ()
  (when (cffi:foreign-symbol-pointer "epoll_create")
    (setf *acceptor-loop* #'acceptor-loop-epoll)))

(eval-when (:load-toplevel :execute)
  (maybe-configure-epoll))

#+nil
(trace
 epoll:create epoll-add epoll-del
 acceptor-loop-epoll make-worker agent-in agent-out agent-error
 stream-flush-output unistd:c-write
 stream-output-index
 )
