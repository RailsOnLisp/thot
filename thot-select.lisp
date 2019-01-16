;;
;;  Thot - http web server
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com> 0614550127
;;

(in-package :thot)

(defun request-loop-simple (request-stream reply-stream addr)
  (with-simple-restart (abort "Abort request loop")
    (loop
       (when *stop*
         (return))
       (handler-bind
           ((warning (lambda (w)
                       (msg warn w)
                       (continue)))
            (error (lambda (e)
                     (msg error e)
                     (unless (debug-p :conditions)
                       (return)))))
         (let* ((req (make-instance 'request :stream request-stream
                                    :remote-addr addr))
                (reply (make-instance 'reply :stream reply-stream))
                (reader (request-reader req reply))
                (result (funcall (the function reader))))
           (stream-flush reply-stream)
           (unless (eq :keep-alive result)
             (return)))))))

(defun fd= (a b)
  (= (the unistd:file-descriptor a) (the unistd:file-descriptor b)))

(defun acceptor-loop-select (fd &optional pipe)
  (declare (type unistd:file-descriptor fd))
  (let ((readfds))
    (push fd readfds)
    (when pipe
      (push (the unistd:file-descriptor pipe) readfds))
    (loop
       (when *stop*
         (return))
       (unistd:with-selected (readfds () () 100)
           (readable writable errors)
         (when (and pipe
                    (find pipe readable :test #'fd=))
           (return))
         (when (find fd readable :test #'fd=)
           (socket:with-accept (clientfd addr) fd
             (let ((request-stream
                    (babel-input-stream
                     (unistd-input-stream clientfd)))
                   (reply-stream
                    (babel-output-stream
                     (multi-buffered-output-stream
                      (unistd-output-stream clientfd)))))
               (request-loop-simple request-stream
                                    reply-stream
                                    (socket:sockaddr-to-string
                                     addr)))))))))

(defun configure-select ()
  (setf *acceptor-loop* #'acceptor-loop-select))

(eval-when (:load-toplevel :execute)
  (configure-select))

;(trace acceptor-loop-select request-loop-simple cffi-socket:accept unistd:write stream-flush stream-flush-output unistd:close)
