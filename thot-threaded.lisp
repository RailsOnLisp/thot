;;
;;  Thot - http web server
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com> 0614550127
;;

(in-package :thot)

(defvar *worker-thread-for-fd*)

(defparameter *init-threads* 8)

(defun make-worker-threads (fd n pipe-in)
  (declare (type fixnum n))
  (let ((threads ())
        (listen-fds ()))
    (dotimes (i n)
      (let ((thread-fd (unistd:dup fd)))
        (push thread-fd listen-fds)
        (push (bordeaux-threads:make-thread
               (funcall *worker-thread-for-fd* thread-fd pipe-in)
               :name "worker")
              threads)))
    (values threads listen-fds)))

(defun join-worker-threads (threads listen-fds pipe-out)
  (setq *stop* t)
  (cffi:with-foreign-object (out :char)
    (setf (cffi:mem-aref out :char) 0)
    (unistd:write pipe-out out 1))
  (dolist (fd listen-fds)
    (unistd:close fd))
  (dolist (thread threads)
    (bordeaux-threads:join-thread thread)))

(defmacro with-worker-threads ((fd count) &body body)
  (let ((threads (gensym "THREADS-"))
        (listen-fds (gensym "LISTEN-FDS-"))
        (pipe-in (gensym "PIPE-IN-"))
        (pipe-out (gensym "PIPE-OUT-")))
    `(unistd:with-pipe (,pipe-in ,pipe-out)
       (multiple-value-bind (,threads ,listen-fds)
           (make-worker-threads ,fd ,count ,pipe-in)
         (unwind-protect (progn ,@body)
           (join-worker-threads ,threads ,listen-fds ,pipe-out))))))

(defun acceptor-loop-threaded (fd)
  (declare (type (unsigned-byte 31) fd))
  (when (debug-p :thot)
    (format t " ~A~%" *worker-thread-for-fd*))
  (set-nonblocking fd)
  (with-worker-threads (fd (1- (the fixnum *init-threads*)))
    (funcall (funcall *worker-thread-for-fd* fd))))

(when bordeaux-threads:*supports-threads-p*
  (unless (boundp '*worker-thread-for-fd*)
    (setq *worker-thread-for-fd* *acceptor-loop*))
  (setq *acceptor-loop* 'acceptor-loop-threaded))

;(untrace start acceptor-loop-threaded request-loop read write set-nonblocking socket:socket socket:bind socket:listen socket:accept unistd:close unistd:select)
