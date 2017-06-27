
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
                        (with-stream (stream (babel-io-stream
                                              (fd-io-stream clientfd)))
                          (request-loop stream)))))))))
    #'acceptor-loop-simple-fun))

(setq *acceptor-loop* 'acceptor-loop-simple)

;(untrace acceptor-loop request-loop read write cffi-socket:accept unistd:close)

