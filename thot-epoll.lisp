
(in-package :thot)

;;  Generic epoll agent class

(defclass agent ()
  ((fd :initarg :fd
       :reader agent-fd
       :type (unsigned-byte 31))))

(defgeneric agent-epoll-events (agent))
(defgeneric agent-error (agent))
(defgeneric agent-in (agent))
(defgeneric agent-out (agent))

;;  Adding an agent

(defvar *epoll-fd*)

(defvar *epoll-agents*
  (make-hash-table))

(defmacro get-agent (fd)
  `(gethash ,fd *epoll-agents*))

(defun set-nonblocking (fd)
  (let ((flags (fcntl:getfl fd)))
    (fcntl:setfl fd (logior +o-nonblock+ flags))))

(defun epoll-add (agent)
  (let ((fd (agent-fd agent)))
    (set-nonblocking fd)
    (setf (get-agent fd) agent)
    (epoll:add *epoll-fd* fd
               (agent-epoll-events agent)
               :data-fd fd)))

;;  Worker agent

(defclass worker (agent)
  ((stream :reader worker-stream
           :type stream)
   (addr :initarg :addr
            :reader worker-addr)
   (request :initform nil
            :accessor worker-request
            :type (or null request))
   (reply :initform nil
          :accessor worker-reply)))

(defmethod agent-epoll-events ((agent worker))
  (logior epoll:+in+ epoll:+out+ epoll:+err+))

(defmethod agent-error ((agent worker))
  (error "worker"))

(defmethod agent-in ((agent worker))
  )

(defmethod agent-out ((agent worker))
  )

;;  Acceptor agent

(defclass acceptor (agent) ())

(defmethod agent-epoll-events ((agent acceptor))
  (logior epoll:+in+ epoll:+err+))

(define-condition accept-error (error)
  ((acceptor :initarg acceptor
             :reader accept-error-acceptor
             :type acceptor)))

(defmethod agent-error ((agent acceptor))
  (error 'accept-error :acceptor agent))

(defmethod agent-in ((agent acceptor))
  (multiple-value-bind (clientfd clientaddr)
      (cffi-sockets:accept (agent-fd agent))
    (let ((worker (make-instance 'worker :fd clientfd :addr clientaddr)))
      (epoll-add worker))))

;;  Thread event loop

(defun event-loop-epoll (acceptfd)
  (epoll:with (*epoll-fd*)
    (let ((acceptor (make-instance 'acceptor :fd acceptfd)))
      (epoll-add acceptor))
    (epoll:wait (events fd *epoll-fd*)
      (let ((agent (get-agent fd)))
        (cond ((not (= 0 (logand epoll:+err+ events)))
               (agent-error agent))
              ((not (= 0 (logand epoll:+in+ events)))
               (agent-in agent))
              ((not (= 0 (logand epoll:+out+ events)))
               (agent-out agent)))))))

;;  
(defun acceptor-loop-epoll (fd)
  (declare (type (unsigned-byte 31) fd))
  )
