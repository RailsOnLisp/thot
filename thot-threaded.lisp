
(in-package :thot)

(defvar *listen-fd*)

(defun worker-thread ()
  ;(format t "~&WORKER THREAD~%")
  (loop
     (when *stop*
       (return))
     (cffi-socket:with-accept (clientfd) *listen-fd*
       (with-stream (stream (babel-io-stream (fd-io-stream clientfd)))
         (request-loop stream)))))

(defparameter *init-threads* 8)

(defvar *worker-threads*)
(defvar *worker-sockfds*)

(defun init-worker-threads (n)
  (loop
     (when (<= n (length *worker-threads*))
       (return))
     (let ((thread (bordeaux-threads:make-thread 'worker-thread
                                                 :name "worker")))
       (push thread *worker-threads*))))

(defun join-worker-threads ()
  (setq *stop* t)
  (bordeaux-set:set-each (lambda (sockfd)
                           (cffi-socket:shutdown sockfd t t))
                         *worker-sockfds*)
  (loop
     (when (endp *worker-threads*)
       (return))
     (let ((thread (pop *worker-threads*)))
       (bordeaux-threads:join-thread thread))))

(defmacro with-worker-threads (count &body body)
  `(let ((*worker-threads* ())
         (*worker-sockfds* (make-instance 'bordeaux-set:set)))
     (init-worker-threads ,count)
     (unwind-protect (progn ,@body)
       (join-worker-threads))))

(defun acceptor-loop-threaded (fd)
  (declare (type (unsigned-byte 31) fd))
  (setq *stop* nil
        *listen-fd* fd)
  (with-worker-threads (1- *init-threads*)
    (worker-thread)))

(when bordeaux-threads:*supports-threads-p*
  (setq *acceptor-loop* 'acceptor-loop-threaded))
