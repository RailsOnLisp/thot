;;
;;  Thot - http web server
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com> 0614550127
;;

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
   :cffi-stat
   :cl-debug
   :cl-stream
   :common-lisp
   :dirent
   :html-entities
   :str
   :unistd-stream)
  (:shadow #:probe-file)
  #.(cl-stream:shadowing-import-from)
  (:export
   #:*disable-threads*
   #:*reply*
   #:*request*
   #:configure-threads
   #:content
   #:end-headers
   #:header
   #:maybe-configure-epoll
   #:msg
   #:reply
   #:reply-content-length
   #:reply-header
   #:reply-headers
   #:reply-headers-sent
   #:reply-status
   #:request
   #:request-header
   #:request-headers
   #:request-http-version
   #:request-method
   #:request-socket
   #:request-url
   #:start
   #:start-threaded
   #:status
   ))
