
(in-package :common-lisp-user)

(defpackage :thot.system
  (:use :common-lisp :asdf))

(in-package :thot.system)

(defsystem "thot"
  :depends-on ("babel"
	       "bordeaux-threads"
	       "cffi-posix"
	       "cffi-sockets-flexi"
	       "cl-debug")
  :components
  ((:file "package")
   (:file "thot" :depends-on ("package"))))
