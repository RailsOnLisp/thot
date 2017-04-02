
(in-package :thot)

(setf (debug-p :thot) t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+crlf+)
    (defconstant +crlf+
      (coerce '(#\Return #\Newline) 'string))))

(defun read-line-crlf (stream)
  (with-output-to-string (out)
    (let ((cr))
      (loop
	 (let ((c (read-char stream)))
	   (when cr
	     (when (char= #\Newline c)
	       (return))
	     (write-char cr out)
	     (setf cr nil))
	   (if (char= #\Return c)
	       (setf cr c)
	       (write-char c out)))))))

(defun read-until (end-char stream)
  "Reads stream into a string until END-CHAR is read.
END-CHAR is not returned as part of the string."
  (with-output-to-string (out)
    (loop
       (let ((c (read-char stream)))
	 (when (char= end-char c)
	   (return))
	 (write-char c out)))))

(defclass request ()
  ((socket :initarg :socket
	   :reader request-socket%
	   :type stream)
   (method :initarg :method
	   :reader request-method%
	   :type symbol)
   (url :initarg :url
	:reader request-url%
	:type string)
   (uri :initform nil
	:accessor request-uri%
	:type string)
   (query :initform nil
	  :accessor request-query%
	  :type string)
   (http-version :initarg :http-version
		 :reader request-http-version%
		 :type string)
   (headers :initform (make-hash-table :test 'equalp :size 32)
	    :reader request-headers%
	    :type hash-table)
   (data :initform nil
	 :accessor request-data%
	 :type string)))

(defvar *request*)

(defun request-socket (&optional (request *request*))
  (declare (type request request))
  (request-socket% request))

(defun request-method (&optional (request *request*))
  (declare (type request request))
  (request-method% request))

(defun request-url (&optional (request *request*))
  (declare (type request request))
  (request-url% request))

(defun split-uri-query (&optional (request *request*))
  (let* ((url (thot:request-url request))
	 (url-? (position #\? url))
	 (uri (if url-?
		  (subseq url 0 url-?)
		  url))
	 (query (if url-?
		    (subseq url url-?)
		    nil)))
    (setf (request-uri% request) uri
	  (request-query% request) query)))
  
(defun request-uri (&optional (request *request*))
  (unless (request-uri% request)
    (split-uri-query request)
    (request-uri% request)))

(defun request-http-version (&optional (request *request*))
  (declare (type request request))
  (request-http-version% request))

(defun request-headers (&optional (request *request*))
  (declare (type request request))
  (request-headers% request))

(defun request-header (name &optional (request *request*))
  (gethash name (request-headers request)))

(defun request-content-length (&optional (request *request*))
  (parse-integer (request-header "Content-Length" request)))

(defun request-data (&optional (request *request*))
  (let* ((length (request-content-length request))
	 (data (make-array length :element-type '(unsigned-byte 8)))
	 (socket (request-socket request))
	 (encoding (flexi-streams:flexi-stream-external-format socket)))
    (read-sequence data socket)
    (babel:octets-to-string data :encoding encoding)))
    
(defun read-spaces (socket)
  (loop
     (unless (char= #\Space (peek-char nil socket))
       (return))
     (read-char socket)))

(defun read-header (socket headers)
  (unless (and (char= #\Return (peek-char nil socket))
	       (read-char socket)
	       (char= #\Newline (read-char socket)))
    (let ((name (read-until #\: socket)))
      (read-spaces socket)
      (let ((value (read-line-crlf socket)))
	(when (debug-p (or :thot :http))
	  (format t "~&thot: ~A: ~A~%" name value))
	(setf (gethash name headers) value)))))

(defun read-request (socket)
  (let* ((method (find-symbol (read-until #\Space socket) :http-method))
	 (url (read-until #\Space socket))
	 (http-version (read-line-crlf socket))
	 (request (make-instance 'request :socket socket :method method
				 :url url :http-version http-version))
	 (headers (request-headers request)))
    (when (debug-p (or :thot :http))
      (format t "~&thot: ~A ~A ~A~%" method url http-version))
    (loop
       (unless (read-header socket headers)
	 (return)))
    request))

(defclass reply ()
  ((status :initform nil
	   :accessor reply-status%)
   (headers :initform nil
	    :accessor reply-headers%)
   (headers-sent :initform nil
		 :accessor reply-headers-sent%)))

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
  (format (request-socket) "~A ~A~A" (request-http-version) line +crlf+))

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
  (format (request-socket) "~A~A" line +crlf+))

(defun end-headers ()
  (unless (reply-headers-sent)
    (setf (reply-headers-sent) t)
    (header "")))

(defun content (string)
  (end-headers)
  (write-string string (request-socket)))

(defun 404-not-found ()
  (status "404 Not found")
  (header "Content-Type: text/plain")
  (content (format nil "404 Not found
  
The requested url ~S was not found on this server."
		   (request-url *request*))))

(defun 404-not-found-handler ()
  '404-not-found)

(defvar *url-handlers*
  '(404-not-found-handler))

(defun handle-request (request)
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
	   (return))))))

(defun request-loop (socket)
  (loop
     (let ((request (read-request socket)))
       (unless request
	 (return))
       (handle-request request)
       (unless (string-equal "keep-alive" (request-header 'connection request))
	 (return)))))

(defun acceptor-loop (fd)
  (declare (type (unsigned-byte 31) fd))
  (loop
     (let ((clientfd (cffi-sockets:accept fd)))
       (unless clientfd
	 (return))
       (let ((socket (cffi-sockets-flexi:make-socket-stream clientfd)))
	 (unwind-protect (request-loop socket)
	   (close socket))))))

(defun start (&key (address "0.0.0.0") (port 8000))
  (let ((fd (cffi-sockets:socket cffi-sockets:+af-inet+
				 cffi-sockets:+sock-stream+
				 0)))
    (unwind-protect
	 (progn
	   (cffi-sockets:bind-inet fd address port)
	   (cffi-sockets:listen-sock fd 128)
	   (acceptor-loop fd))
      (cffi-sockets:close-sock fd))))

(defvar *pipe-in*)
(defvar *pipe-out*)

(defun read-fd ()
  (with-foreign-object (fd :int)
    (let ((r (c-read *pipe-in* fd (foreign-type-size :int))))
      (when (< r 0)
	(error-errno "read"))
      (when (< 0 r)
	(mem-aref fd :int)))))

(defun write-fd (fd)
  (with-foreign-object (fd% :int)
    (setf (mem-aref fd% :int) fd)
    (let ((w (c-write *pipe-out* fd% (foreign-type-size :int))))
      (when (< w 0)
	(error-errno "write"))
      (when (< 0 w)
	fd))))

(trace read-fd write-fd)

(defun worker-thread ()
  (loop
     (let ((sockfd (read-fd)))
       (unless sockfd
	 (return))
       (let ((socket (cffi-sockets-flexi:make-socket-stream sockfd)))
	 (unwind-protect (request-loop socket)
	   (close socket))))))

(defparameter *init-threads* 2)

(defvar *threads*)

(defun init-threads (n)
  (loop
     (when (<= (length *threads*) n)
       (return))
     (push (bordeaux-threads:make-thread #'worker-thread) *threads*)))

(defun join-threads ()
  (loop
     (when (endp *threads*)
       (return))
     (let ((thread (pop *threads*)))
       (bordeaux-threads:join-thread thread))))
  
(defun acceptor-loop-threaded (fd)
  (declare (type (unsigned-byte 31) fd))
  (loop
     (let ((clientfd (ignore-errors (cffi-sockets:accept fd))))
       (unless clientfd
	 (return))
       (format t "~&accept ~D~%" clientfd)
       (write-fd clientfd))))

(defun start-threaded (&key (address "0.0.0.0") (port 8000))
  (let ((fd (cffi-sockets:socket cffi-sockets:+af-inet+
				 cffi-sockets:+sock-stream+
				 0)))
    (unwind-protect
	 (progn
	   (cffi-sockets:bind-inet fd address port)
	   (cffi-sockets:listen-sock fd 128)
	   (multiple-value-bind (*pipe-in* *pipe-out*) (pipe)
	     (let ((*threads*))
	       (init-threads *init-threads*)
	       (unwind-protect (acceptor-loop-threaded fd)
		 (join-threads)))))
      (cffi-sockets:close-sock fd))))
