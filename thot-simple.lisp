;;
;;  Thot - http web server
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com> 0614550127
;;

(in-package :thot)

(defun acceptor-loop-simple (fd &optional pipe)
  (declare (type (unsigned-byte 31) fd))
  (let ((readfds))
    (push fd readfds)
    (when pipe
      (push pipe readfds))
    (labels ((acceptor-loop-simple-fun ()
               (loop
                  (when *stop*
                    (return))
                  (unistd:with-selected (readfds () () 100)
                      (readable writable errors)
                    (when (and pipe (find pipe readable))
                      (return))
                    (when (find fd readable)
                      (socket:with-accept (clientfd) fd
                        (with-stream (request-stream
                                      (babel-input-stream
                                       (fd-input-stream clientfd)))
                          (with-stream (reply-stream
                                        (babel-output-stream
                                         (multi-buffered-output-stream
                                          (fd-output-stream clientfd))))
                            (request-loop request-stream reply-stream)))))))))
      #'acceptor-loop-simple-fun)))

(setq *acceptor-loop* 'acceptor-loop-simple)

;(untrace acceptor-loop request-loop read write cffi-socket:accept unistd:close)

