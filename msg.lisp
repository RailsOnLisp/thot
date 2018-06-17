
(in-package :thot)

(defmacro msg (level &rest parts)
  `(progn
     (fresh-line)
     (write-str *standard-output*
                ,(symbol-name level)
                #\Space
                ,@parts
                #\Newline)
     (force-output)))
