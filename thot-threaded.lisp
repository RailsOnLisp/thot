
(in-package :thot)

(defvar *worker-thread-for-fd*)

(defparameter *init-threads* 8)

(defun make-worker-threads (fd n)
  (let ((threads ())
        (listen-fds ()))
    (dotimes (i n)
      (let ((thread-fd (unistd:dup fd)))
        (push thread-fd listen-fds)
        (push (bordeaux-threads:make-thread
               (funcall *worker-thread-for-fd* thread-fd)
               :name "worker")
              threads)))
    (values threads listen-fds)))

(defun join-worker-threads (threads listen-fds)
  (setq *stop* t)
  (dolist (fd listen-fds)
    (unistd:close fd))
  (dolist (thread threads)
    (bordeaux-threads:join-thread thread)))

(defmacro with-worker-threads ((fd count) &body body)
  (let ((threads (gensym "THREADS-"))
        (listen-fds (gensym "LISTEN-FDS-")))
    `(multiple-value-bind (,threads ,listen-fds)
         (make-worker-threads ,fd ,count)
       (unwind-protect (progn ,@body)
         (join-worker-threads ,threads ,listen-fds)))))

(defun acceptor-loop-threaded (fd)
  (declare (type (unsigned-byte 31) fd))
  (set-nonblocking fd)
  (with-worker-threads (fd (1- *init-threads*))
    (funcall (funcall *worker-thread-for-fd* fd))))

(when bordeaux-threads:*supports-threads-p*
  (unless (boundp '*worker-thread-for-fd*)
    (setq *worker-thread-for-fd* *acceptor-loop*))
  (setq *acceptor-loop* 'acceptor-loop-threaded))

(untrace start acceptor-loop-threaded request-loop read write
       set-nonblocking
       socket:socket socket:bind socket:listen socket:accept
       unistd:close unistd:select)
