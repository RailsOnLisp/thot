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
                        (let ((request-stream
                               (babel-input-stream
                                (unistd-input-stream clientfd)))
                              (reply-stream
                               (babel-output-stream
                                (multi-buffered-output-stream
                                 (unistd-output-stream clientfd)))))
                          (request-loop request-stream reply-stream)
                          (stream-flush reply-stream))))))))
      #'acceptor-loop-simple-fun)))

(setq *acceptor-loop* 'acceptor-loop-simple)

;(trace acceptor-loop-simple request-loop cffi-socket:accept unistd:write stream-flush stream-flush-output unistd:close)
