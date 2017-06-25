
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
            :type 'request)
   (stream :initarg :stream
           :reader worker-stream
           :type stream)))

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
        (cond ((eq :eof result) (epoll-del worker))
              ((eq nil result) (setf (worker-reader-cont worker) nil))
              ((eq :keep-alive result) (setf (worker-keep-alive worker) t))
              ((functionp result) (setf (worker-reader-cont worker) result))
              (t (error "worker input error ~S" worker)))))))

(defmethod agent-out ((worker worker) (epoll-fd fixnum))
  (let ((reply (worker-reply worker)))
    ))

;;  Acceptor agent

(defclass acceptor (agent)
  ())

(defmethod agent-epoll-events ((agent acceptor))
  (logior epoll:+in+ epoll:+err+))

(define-condition acceptor-error (agent-error)
  ())

(defmethod agent-error ((acceptor acceptor) (epoll-fd fixnum))
  (error 'acceptor-error :agent acceptor))

(defmethod agent-in ((acceptor acceptor) (epoll-fd fixnum))
  (multiple-value-bind (fd addr) (socket:accept (agent-fd acceptor))
    (unless (eq :non-blocking fd)
      (let* ((stream (babel-io-stream (fd-io-stream fd)))
             (request (make-instance 'request :stream stream))
             (reply (make-instance 'reply))
             (worker (make-instance 'worker
                                    :addr addr
                                    :fd fd
                                    :reader-cont (request-reader
                                                  request reply #'request-cont)
                                    :request request
                                    :reply reply
                                    :stream stream)))
        (epoll-add epoll-fd worker)))))

;;  Thread event loop

(defun event-loop-epoll (acceptfd)
  (epoll:with (epoll-fd)
    (let ((acceptor (make-instance 'acceptor :fd acceptfd)))
      (epoll-add epoll-fd acceptor))
    (epoll:wait (events fd epoll-fd)
      (let ((agent (get-agent fd)))
        (cond ((not (= 0 (logand epoll:+err+ events)))
               (agent-error agent epoll-fd))
              ((not (= 0 (logand epoll:+in+ events)))
               (agent-in agent epoll-fd))
              ((not (= 0 (logand epoll:+out+ events)))
               (agent-out agent epoll-fd)))))))

;;

(defun acceptor-loop-epoll (fd)
  (declare (type (unsigned-byte 31) fd))
  )
