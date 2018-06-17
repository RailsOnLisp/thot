;;
;;  Thot - http web server
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com> 0614550127
;;

(in-package :thot)

(defparameter *init-threads* 8)

(defun make-worker-threads (fd n pipe-in)
  (declare (type fixnum n))
  (let ((threads ())
        (listen-fds ()))
    (dotimes (i n)
      (let ((thread-fd (unistd:dup fd)))
        (push thread-fd listen-fds)
        (push (bordeaux-threads:make-thread
               (funcall *acceptor-loop* thread-fd pipe-in)
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

(defun main-loop-threaded (fd)
  (declare (type unistd:file-descriptor fd))
  (msg info " " *acceptor-loop*)
  (set-nonblocking fd)
  (with-worker-threads (fd (1- (the fixnum *init-threads*)))
    (funcall *acceptor-loop* fd)))

(defvar *disable-threads* nil)

(defun threaded-p ()
  (and bordeaux-threads:*supports-threads-p*
       (not *disable-threads*)))

(defun maybe-configure-threaded ()
  (when (threaded-p)
    (setf *main-loop* #'main-loop-threaded)))

(eval-when (:load-toplevel :execute)
  (maybe-configure-threaded))

;(untrace start acceptor-loop-threaded read write set-nonblocking socket:socket socket:bind socket:listen socket:accept unistd:close unistd:select)
