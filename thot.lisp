
(in-package :thot)

(setf (debug-p :thot) nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+crlf+)
    (defconstant +crlf+
      (coerce '(#\Return #\Newline) 'string))))

;;  Request

(defclass request ()
  ((stream :initarg :stream
	   :reader request-stream%
	   :type stream)
   (method :initarg :method
	   :accessor request-method%
	   :type symbol)
   (target :initarg :target
           :accessor request-target%
           :type string)
   (http-version :initarg :http-version
                 :accessor request-http-version%
                 :type string)
   (headers :initform (make-hash-table :test 'equalp :size 32)
	    :reader request-headers%
	    :type hash-table)
   (uri :accessor request-uri%
	:type string)
   (query :initform nil
	  :accessor request-query%
	  :type string)))

(defvar *request*)

(defun request-stream (&optional (request *request*))
  (declare (type request request))
  (request-stream% request))

(defun request-method (&optional (request *request*))
  (declare (type request request))
  (request-method% request))

(defun request-target (&optional (request *request*))
  (declare (type request request))
  (request-target% request))

(defun request-http-version (&optional (request *request*))
  (declare (type request request))
  (request-http-version% request))

(defun request-headers (&optional (request *request*))
  (declare (type request request))
  (request-headers% request))

(defun split-request-uri-and-query (request)
  (declare (type request request))
  (let* ((target (request-target% request))
	 (target-? (position #\? target))
	 (uri (if target-?
		  (subseq target 0 target-?)
		  target))
	 (query (if target-?
		    (subseq target target-?)
		    nil)))
    (setf (request-uri% request) uri
	  (request-query% request) query)))

(defun request-uri (&optional (request *request*))
  (declare (type request request))
  (unless (slot-boundp request 'uri)
    (split-request-uri-and-query request))
  (slot-value request 'uri))

(defun request-query (&optional (request *request*))
  (declare (type request request))
  (unless (slot-boundp request 'query)
    (split-request-uri-and-query request))
  (slot-value request 'query))

(defun request-header (header-name &optional (request *request*))
  (declare (type request request))
  (gethash header-name (request-headers% request)))

(defsetf request-header (header-name &optional (request '*request*)) (value)
  `(setf (gethash ,header-name (request-headers ,request)) ,value))

(defun request-content-length (&optional (request *request*))
  (parse-integer (request-header "Content-Length" request)))

;;  HTTP parser

(defmacro with-readers-for (stream definitions &body body)
  (declare (type symbol stream))
  (flet ((reader (definition)
           (destructuring-bind (name (element) &rest body) definition
             (declare (type symbol name element))
             (let ((state (gensym "STATE-")))
               `(,name ()
                  (multiple-value-bind (,element ,state) (read ,stream)
                    (case ,state
                      ((nil) ,@body)
                      ((:eof) :eof)
                      ((:non-blocking) #',name)
                      (otherwise (error 'stream-input-error
                                        :stream ,stream)))))))))
    `(labels ,(mapcar #'reader definitions)
       ,@body)))

(defun request-reader (stream cont)
  (let ((request (make-instance 'request :stream stream))
        (buffer (string-output-stream))
        (name "")
        (value ""))
    (flet ((get-buffer ()
             (prog1 (string-output-stream-string buffer)
               (sequence-output-stream-reset buffer))))
      (with-readers-for stream
          ((method (char)
             (cond ((char= #\Space char)
                    (setf (request-method% request) (get-buffer))
                    (target))
                   (t (write buffer char)
                      (method))))
           (target (char)
             (cond ((char= #\Space char)
                    (setf (request-target% request) (get-buffer))
                    (version))
                   (t (write buffer char)
                      (target))))
           (version (char)
             (cond ((char= #\Return char)
                    (setf (request-http-version% request) (get-buffer))
                    (version-lf))
                   (t (write buffer char)
                      (version))))
           (version-lf (char)
             (cond ((char= #\Newline char)
                    (when (debug-p (or :thot :http))
                      (format t "~&thot: ~A ~A ~A~%"
                              (request-method% request)
                              (request-target% request)
                              (request-http-version% request)))
                    (next-header))
                   (t (error "Missing request line LF"))))
           (next-header (char)
             (cond ((char= #\Return char) (end-of-headers))
                   (t (write buffer char)
                      (header-name))))
           (header-name (char)
             (cond ((char= #\: char)
                    (setq name (get-buffer))
                    (header-spaces))
                   (t (write buffer char)
                      (header-name))))
           (header-spaces (char)
             (cond ((char= #\Space char) (header-spaces))
                   (t (write buffer char)
                      (header-value))))
           (header-value (char)
             (cond ((char= #\Return char)
                    (setq value (get-buffer))
                    (header-lf))
                   (t (write buffer char)
                      (header-value))))
           (header-lf (char)
             (cond ((char= #\Newline char)
                    (when (debug-p (or :thot :http))
                      (format t "~&thot: ~A: ~A~%" name value))
                    (setf (request-header name request) value)
                    (next-header))
                   (t (error "Missing header LF"))))
           (end-of-headers (char)
             (cond ((char= #\Newline char) (funcall cont request))
                   (t (error "Missing end of headers LF")))))
        #'method))))

;;  Reply

(defclass reply ()
  ((status :initform nil
	   :accessor reply-status%)
   (headers :initform nil
	    :accessor reply-headers%)
   (headers-sent :initform nil
		 :accessor reply-headers-sent%
                 :type boolean)))

(defvar *reply*)

(defun reply-status (&optional (reply *reply*))
  (declare (type reply reply))
  (reply-status% reply))

(defsetf reply-status (&optional (reply '*reply*)) (value)
  `(setf (reply-status% ,reply) ,value))

(defun reply-headers (&optional (reply *reply*))
  (declare (type reply reply))
  (reply-headers% reply))

(defsetf reply-headers (&optional (reply '*reply*)) (value)
  `(setf (reply-headers% ,reply) ,value))

(defun reply-header (name &optional (reply *reply*))
  (declare (type reply reply))
  (let ((header (assoc name (reply-headers reply) :test #'string-equal)))
    (when header
      (rest header))))

(defun reply-headers-sent (&optional (reply *reply*))
  (declare (type reply reply))
  (reply-headers-sent% reply))

(defsetf reply-headers-sent (&optional (reply '*reply*)) (value)
  `(setf (reply-headers-sent% ,reply) ,value))

(defun reply-content-length (&optional (reply *reply*))
  (declare (type reply reply))
  (parse-integer (reply-header 'content-length reply)))

(defun status (line)
  (let ((status (reply-status)))
    (when status
      (error 'status-already-sent status line)))
  (setf (reply-status) line)
  (let ((stream (request-stream)))
    (write-sequence stream (request-http-version))
    (write stream #\Space)
    (write-sequence stream line)
    (write-sequence stream +crlf+)))

(defun header (line)
  (unless (reply-status)
    (status "200 OK"))
  (let ((headers (reply-headers)))
    (if (endp headers)
	(setf (reply-headers) (list line))
	(loop
	   (when (endp (rest headers))
	     (setf (rest headers) (list line))
	     (return))
	   (pop headers))))
  (let ((stream (request-stream)))
    (write-sequence stream line)
    (write-sequence stream +crlf+)))

(defun end-headers ()
  (unless (reply-headers-sent)
    (setf (reply-headers-sent) t)
    (header "")))

(defun content (string)
  (end-headers)
  (write-sequence (request-stream) string))

(defun 404-not-found ()
  (status "404 Not found")
  (header "Content-Type: text/plain")
  (content (format nil "404 Not found
  
The requested url ~S was not found on this server."
		   (request-target))))

(defun 404-not-found-handler ()
  '404-not-found)

(defvar *url-handlers*
  '(404-not-found-handler))

(defun request-cont (request)
  (let ((handlers *url-handlers*)
        (*request* request)
	(*reply* (make-instance 'reply)))
    (loop
       (when (endp handlers)
	 (return))
       (let* ((handler-func (pop handlers))
	      (handler (funcall handler-func)))
	 (when handler
	   (when (debug-p (or :thot :http))
	     (format t "~&~S -> ~S~%" handler-func handler))
	   (funcall handler)
           (flush (request-stream))
	   (return))))
    (if (string-equal "keep-alive" (request-header 'connection))
        :keep-alive
        nil)))

(defvar *stop* nil)

(defun request-loop (stream)
  (loop
     (when *stop*
       (return))
     (unless (eq :keep-alive
                 (funcall (request-reader stream #'request-cont)))
       (return))))

(defvar *acceptor-loop*)

(defun start (&key (address "0.0.0.0") (port 8000))
  (cffi-socket:with-socket (fd cffi-socket:+af-inet+
                               cffi-socket:+sock-stream+
                               0)
    (cffi-socket:bind-inet fd address port)
    (cffi-socket:listen fd 128)
    (funcall *acceptor-loop* fd)))
