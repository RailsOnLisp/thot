
(in-package :thot)

(defvar *listen-fds*)

(defun worker-thread (fd &optional dup)
  (let ((listen-fd (if dup (unistd:dup fd) fd)))
    (when dup
      (pushnew listen-fd *listen-fds*))
    (labels ((worker-thread-fd ()
               ;;(format t "~&WORKER THREAD~%")
               (unwind-protect
                    (loop
                       (when *stop*
                         (return))
                       (ignore-errors
                         (unistd:with-selected (`(,listen-fd) () () 1)
                             (readable writable errors)
                           (when readable
                             (socket:with-accept (clientfd) listen-fd
                               (with-stream (stream (babel-io-stream
                                                     (fd-io-stream clientfd)))
                                 (request-loop stream)))))))
                 (when dup
                   (unistd:close listen-fd)))))
      #'worker-thread-fd)))

(defparameter *init-threads* 8)

(defun make-worker-threads (fd n)
  (let ((threads ()))
    (dotimes (i n)
      (push (bordeaux-threads:make-thread
             (worker-thread fd t)
             :name "worker")
            threads))
    threads))

(defun join-worker-threads (threads)
  (setq *stop* t)
  (dolist (thread threads)
    (bordeaux-threads:join-thread thread)))

(defmacro with-worker-threads ((fd count) &body body)
  (let ((threads (gensym "THREADS-")))
    `(let* ((*listen-fds* ())
            (,threads (make-worker-threads ,fd ,count)))
       (unwind-protect (progn ,@body)
         (join-worker-threads ,threads)))))

(defun acceptor-loop-threaded (fd)
  (declare (type (unsigned-byte 31) fd))
  (set-nonblocking fd)
  (with-worker-threads (fd (1- *init-threads*))
    (funcall (worker-thread fd))))

(when bordeaux-threads:*supports-threads-p*
  (setq *acceptor-loop* 'acceptor-loop-threaded))

(untrace start acceptor-loop-threaded request-loop read write
       set-nonblocking
       socket:socket socket:bind socket:listen socket:accept
       unistd:close unistd:select)
