
(in-package :thot)

(defun acceptor-loop (fd)
  (declare (type (unsigned-byte 31) fd))
  (loop
     (when *stop*
       (return))
     (cffi-socket:with-accept (clientfd) fd
       (with-stream (stream (babel-io-stream (fd-io-stream clientfd)))
         (request-loop stream)))))

(setq *acceptor-loop* 'acceptor-loop)

;(untrace acceptor-loop request-loop read write cffi-socket:accept unistd:close)
