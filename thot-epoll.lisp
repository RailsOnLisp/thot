
(in-package :thot)

;;  Generic epoll agent class

(defclass agent ()
  ((fd :initarg :fd
       :reader agent-fd
       :type (unsigned-byte 31))))

(defgeneric agent-epoll-events (agent))
(defgeneric agent-error (agent epoll-fd))
(defgeneric agent-in (agent epoll-fd))
(defgeneric agent-out (agent epoll-fd))

(define-condition agent-error (error)
  ((agent :initarg :agent
          :reader agent-error-agent
          :type agent)))

;;  Adding an agent

(defvar *epoll-agents*
  (make-hash-table))

(defmacro get-agent (fd)
  `(gethash ,fd *epoll-agents*))

(defun remove-agent (fd)
  (remhash fd *epoll-agents*))

(defun epoll-add (epoll-fd agent)
  (let ((fd (agent-fd agent)))
    (set-nonblocking fd)
    (setf (get-agent fd) agent)
    (epoll:add epoll-fd fd
               (agent-epoll-events agent)
               :data-fd fd)))

(defun epoll-del (epoll-fd agent)
  (let ((fd (agent-fd agent)))
    (epoll:del epoll-fd fd)
    (socket:shutdown fd t t)
    (unistd:close fd)
    (remove-agent fd)))

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
          :type 'reply)
   (request :initarg :request
            :accessor worker-request
            :type 'request)))

(defmethod agent-epoll-events ((worker worker))
  (logior epoll:+in+ epoll:+out+ epoll:+err+))

(define-condition worker-error (agent-error)
  ())

(defmethod agent-error ((worker worker) (epoll-fd fixnum))
  (error 'worker-error :agent worker))

(defmethod agent-in ((worker worker) (epoll-fd fixnum))
  (let ((reader-cont (worker-reader-cont worker)))
    (when reader-cont
      (let ((result (funcall reader-cont)))
        (cond ((eq :eof result) (epoll-del epoll-fd worker))
              ((eq nil result) (setf (worker-reader-cont worker) nil))
              ((eq :keep-alive result) (setf (worker-keep-alive worker) t
                                             (worker-reader-cont worker) nil)
               :keep-alive)
              ((functionp result) (setf (worker-reader-cont worker) result))
              (t (error "worker input error ~S" worker)))))))

(defmethod agent-out ((worker worker) (epoll-fd fixnum))
  (let* ((request (worker-request worker))
         (reply (worker-reply worker))
         (babel-stream (reply-stream reply))
         (stream (stream-underlying-stream babel-stream)))
    (cond ((= 0 (stream-output-length stream))
           (cond ((worker-keep-alive worker)
                  ;; read request body
                  (setf (worker-reader-cont worker)
                        (request-reader (reset-request request)
                                        (reset-reply reply)
                                        #'request-cont))
                  (agent-in worker epoll-fd))
                 (t
                  (epoll-del epoll-fd worker))))
          (t
           (case (stream-flush-output-buffer stream)
             ((nil) nil)
             ((:eof) (epoll-del epoll-fd worker))
             ((:non-blocking) :non-blocking)
             (otherwise (error 'stream-output-error :stream stream)))))))

;;  Acceptor agent

(defclass acceptor (agent)
  ())

(defmethod agent-epoll-events ((agent acceptor))
  (logior epoll:+in+ epoll:+err+))

(define-condition acceptor-error (agent-error)
  ())

(defmethod agent-error ((acceptor acceptor) (epoll-fd fixnum))
  (error 'acceptor-error :agent acceptor))

(defun make-worker (fd addr)
  (let* ((request-stream (babel-input-stream (fd-input-stream fd)))
         (reply-stream (babel-output-stream
                        (multi-buffered-output-stream
                         (fd-output-stream fd))))
         (request (make-instance 'request :stream request-stream))
         (reply (make-instance 'reply :stream reply-stream))
         (reader-cont (request-reader request reply #'request-cont)))
    (make-instance 'worker
                   :addr addr
                   :fd fd
                   :reader-cont reader-cont
                   :request request
                   :reply reply)))

(defmethod agent-in ((acceptor acceptor) (epoll-fd fixnum))
  (multiple-value-bind (fd addr) (socket:accept (agent-fd acceptor))
    (unless (eq :non-blocking fd)
      (epoll-add epoll-fd (make-worker fd addr)))))

(defclass control (agent)
  ())

(defmethod agent-epoll-events ((agent control))
  epoll:+in+)

(defmethod agent-in ((agent control) (epoll-fd fixnum))
  (setq *stop* t))

;;  Thread event loop

(defun acceptor-loop-epoll (listenfd &optional pipe)
  (declare (type (unsigned-byte 31) listenfd))
  (labels ((acceptor-loop-epoll-fun ()
             (epoll:with (epoll-fd)
               (epoll-add epoll-fd (make-instance 'acceptor :fd listenfd))
               (when pipe
                 (epoll-add epoll-fd (make-instance 'control :fd pipe)))
               (loop
                  (when *stop*
                    (return))
                  (epoll:wait (events fd epoll-fd)
                    (let ((agent (get-agent fd)))
                      (unless agent (error "bad epoll fd ~S" fd))
                      (cond ((not (= 0 (logand epoll:+err+ events)))
                             (agent-error agent epoll-fd))
                            ((not (= 0 (logand epoll:+in+ events)))
                             (agent-in agent epoll-fd))
                            ((not (= 0 (logand epoll:+out+ events)))
                             (agent-out agent epoll-fd)))))))))
    #'acceptor-loop-epoll-fun))

(when (cffi:foreign-symbol-pointer "epoll_create")
  (cond ((eq *acceptor-loop* 'acceptor-loop-simple)
         (setq *acceptor-loop* 'acceptor-loop-epoll))
        ((eq *acceptor-loop* 'acceptor-loop-threaded)
         (setq *worker-thread-for-fd* 'acceptor-loop-epoll))))

(trace socket:socket socket:bind socket:listen socket:accept
       unistd:close
       epoll-add epoll-del
       acceptor-loop-epoll make-worker agent-in agent-out)
