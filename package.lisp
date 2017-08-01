
(in-package :common-lisp)

(defpackage :http-method
  (:export
   #:delete
   #:get
   #:head
   #:post
   #:put
   #:connect
   #:options
   #:trace
   #:patch))

(defpackage :thot
  (:use
   :babel-stream
   :bordeaux-threads
   :cffi
   :cffi-errno
   :cl-debug
   :cl-stream
   :common-lisp
   :dirent
   :fd-stream
   :html-entities)
  #.(cl-stream:shadowing-import-from)
  (:export
   #:request
   #:*request*
   #:request-socket
   #:request-method
   #:request-url
   #:request-http-version
   #:request-headers
   #:request-header
   #:reply
   #:*reply*
   #:reply-status
   #:reply-headers
   #:reply-header
   #:reply-headers-sent
   #:reply-content-length
   #:status
   #:header
   #:end-headers
   #:content
   #:start
   #:start-threaded))
