
(in-package :thot)

(defmacro msg (level &rest parts)
  `(progn
     (write-str *standard-output*
                ,(symbol-name level)
                #\Space
                ,@parts
                #\Newline)
     (force-output)))
