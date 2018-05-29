
(in-package :common-lisp-user)

(defpackage :thot.system
  (:use :common-lisp :asdf))

(in-package :thot.system)

(defsystem "thot"
  :depends-on ("babel"
               "bordeaux-queue"
               "bordeaux-set"
               "bordeaux-threads"
               "babel-stream"
               "cffi-dirent"
               #+linux "cffi-epoll"
               "cffi-socket"
               "cl-debug"
               "cl-stream"
               "html-entities"
               "rol-uri"
               "unistd-stream")
  :components
  ((:file "package")
   (:file "thot" :depends-on ("package"))
   (:file "thot-simple" :depends-on ("thot"))
   #+threads (:file "thot-threaded" :depends-on ("thot-simple"))
   #+linux (:file "thot-epoll" :depends-on ("thot-threaded"))))
