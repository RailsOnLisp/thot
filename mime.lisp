
(in-package :thot)

(defpackage :file-extensions
  #.(let (symbols)
      (when (find-package :file-extensions)
	(do-external-symbols (s :file-extensions)
	  (push s symbols)))
      `(:export ,@symbols)))

(defgeneric ext (x))

(defmethod ext ((x string))
  (cond ((string= "" x))
        ((char= #\. (char x 0))
         (let ((sym (intern x :file-extensions)))
           (export sym :file-extensions)
           sym))
        (t
         (ext (str #\. x)))))

(defmethod ext ((x symbol))
  (ext (symbol-name x)))

(defun mime-type-p (symbol)
  (find #\/ (symbol-name symbol)))

(defvar *mime-types*
  (make-hash-table :test 'eq))

(defmacro mime-type (ext)
  `(gethash ,ext *mime-types*))

(defun safe-read (stream eof)
  (let ((*read-eval* nil)
        (*readtable* (copy-readtable nil)))
    (flet ((read-comment (stream char)
             (declare (ignore char))
             (cl:read-line stream)
             (cl:read stream nil nil t)))
      (set-macro-character #\# #'read-comment)
      (cl:read stream nil eof))))

(defun load-mime.types (path)
  (let ((in (cl:open path)))
    (unwind-protect
         (let ((eof (gensym))
               (mime-type nil))
           (loop
              (let ((sym (safe-read in eof)))
                (cond ((eq eof sym) (return))
                      ((not (symbolp sym)))
                      ((mime-type-p sym) (setf mime-type sym))
                      (mime-type (let ((ext (ext sym)))
                                   (format t "~S ~S~%" ext mime-type)
                                   (setf (mime-type ext)
                                         mime-type)))))))
      (cl:close in))))

#+openbsd
(load-mime.types "/usr/share/misc/mime.types")
