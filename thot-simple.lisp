
(in-package :thot)

(defun acceptor-loop-simple (fd)
  (declare (type (unsigned-byte 31) fd))
  (labels ((acceptor-loop-simple-fun ()
             (loop
                (when *stop*
                  (return))
                (ignore-errors
                  (unistd:with-selected (`(,fd) () () 1)
                      (readable writable errors)
                    (when readable
                      (socket:with-accept (clientfd) fd
                        (with-stream (stream (babel-io-stream
                                              (fd-io-stream clientfd)))
                          (request-loop stream)))))))))
    #'acceptor-loop-simple-fun))

(setq *acceptor-loop* 'acceptor-loop-simple)

;(untrace acceptor-loop request-loop read write cffi-socket:accept unistd:close)

