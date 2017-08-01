
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
   (scheme :type string)
   (host :type string)
   (dir :type string)
   (query :initform nil
          :accessor request-query%
          :type string)))

(defvar *request*)

(defun reset-request (&optional (request *request*))
  (declare (type request request))
  (setf (request-method% request) nil
        (request-target% request) nil
        (request-http-version% request) nil
        (request-uri% request) nil
        (request-query% request) nil)
  (clrhash (request-headers% request))
  request)

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

(defun uri-scheme (uri)
  (let ((column (position #\: uri)))
    (unless (null column)
      (subseq uri 0 column))))

(defun request-scheme (&optional (request *request*))
  (declare (type request request))
  (unless (slot-boundp request 'scheme)
    (setf (slot-value request 'scheme)
          (uri-scheme (request-uri request))))
  (slot-value request 'scheme))

(defun uri-host (uri)
  (let ((host (search "://" uri)))
    (unless (null host)
      (incf host 3)
      (let ((host-end (position #\/ uri :start host)))
        (subseq uri host host-end)))))

(defun request-host (&optional (request *request*))
  (declare (type request request))
  (unless (slot-boundp request 'host)
    (setf (slot-value request 'host)
          (uri-host (request-uri request))))
  (slot-value request 'host))

(defun uri-dir (uri)
  (let ((dir-end (position #\/ uri :from-end t :start 1)))
    (subseq uri 0 dir-end)))

(defun request-dir (&optional (request *request*))
  (declare (type request request))
  (unless (slot-boundp request 'dir)
    (setf (slot-value request 'dir)
          (uri-dir (request-uri request))))
  (slot-value request 'dir))

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

(defun request-reader (request reply cont)
  "Returns a HTTP request reader function which itself returns either :
 :EOF if end of stream was reached, or
 a closure if read would block, or
 the return value of CONT, which should be either :
  :KEEP-ALIVE if the connection is to be kept alive for next request, or
  NIL if the request stream is to be closed after reply."
  (let ((stream (request-stream request))
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
             (cond ((char= #\Newline char) (funcall cont request reply))
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
                 :type boolean)
   (stream :initarg :stream
           :accessor reply-stream%
           :type buffered-output-stream)))

(defvar *reply*)

(defun reset-reply (&optional (reply *reply*))
  (declare (type reply reply))
  (setf (reply-status% reply) nil
        (reply-headers% reply) nil
        (reply-headers-sent% reply) nil)
  reply)

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

(defun reply-header (name &optional (reply *reply*))  (declare (type reply reply))
  (let ((header (assoc name (reply-headers reply) :test #'string-equal)))
    (when header
      (rest header))))

(defun reply-headers-sent (&optional (reply *reply*))
  (declare (type reply reply))
  (reply-headers-sent% reply))

(defsetf reply-headers-sent (&optional (reply '*reply*)) (value)
  `(setf (reply-headers-sent% ,reply) ,value))

(defun reply-stream (&optional (reply *reply*))
  (declare (type reply reply))
  (reply-stream% reply))

(defun reply-content-length (&optional (reply *reply*))
  (declare (type reply reply))
  (parse-integer (reply-header 'content-length reply)))

(defun status (line)
  (let ((status (reply-status)))
    (when status
      (error 'status-already-sent status line)))
  (setf (reply-status) line)
  (let ((stream (reply-stream)))
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
  (let ((stream (reply-stream)))
    (write-sequence stream line)
    (write-sequence stream +crlf+)))

(defun end-headers ()
  (unless (reply-headers-sent)
    (setf (reply-headers-sent) t)
    (header "")))

(defun content (string)
  (end-headers)
  (write-sequence (reply-stream) string))

(defun 404-not-found ()
  (status "404 Not found")
  (header "Content-Type: text/plain")
  (content (format nil "404 Not found

The requested url ~S was not found on this server."
                   (request-target))))

(defun 404-not-found-handler ()
  '(404-not-found))

(defun str (&rest parts)
  (with-output-to-string (s)
    (dolist (p parts)
      (write-sequence s p))))

(defun h (string)
  (expand-entities string))

(defun url-encode (string)
  (rol-uri:%-encode string))

(defun directory-index (local remote)
  (let* ((request-dir (request-dir))
         (dir (if (< (length request-dir)
                     (length remote))
                  (subseq request-dir (length remote))
                  request-dir)))
    (header "Content-Type: text/html")
    (content
     (with-output-to-string (o)
       (flet ((w (&rest parts)
                (dolist (p parts)
                  (write-sequence o p))))
       (w "<html>
 <head>
 </head>
 <body>
  <h1>" (h dir) "</h1>
  <ul>
")
       (let* ((localdir (str local "/" dir))
              (entries (dir localdir)))
         (dolist (df entries)
           (let* ((name (dirent-name df))
                  (slash (if (= +dt-dir+ (dirent-type df)) "/" ""))
                  (url (str (unless (string= "/" remote)
                              (str (url-encode remote) "/"))
                            (unless (string= "/" dir)
                              (url-encode dir)) "/"
                            (url-encode name) slash)))
             (w "   <li><a href=\"" (h url) "\">" (h name) slash "</a></li>
"))))
       (w "  </ul>
 </body>
</html>
"))))))

(defun directory-handler (local remote)
  `(directory-index ,local ,remote))

(defparameter *url-handlers*
  '((directory-handler "/" "/")
    (404-not-found-handler)))

(defun call (list)
  (apply (first list) (rest list)))

(defun request-cont (request reply)
  (let ((handlers *url-handlers*)
        (*request* request)
        (*reply* reply))
    (handler-bind
        ((errno:errno-error (lambda (condition)
                              (cond ((= errno:+epipe+
                                        (errno:errno-error-errno condition))
                                     (return-from request-cont))))))
      (loop
         (when (endp handlers)
           (return))
         (let* ((handler-form (pop handlers))
                (handler (call handler-form)))
           (when handler
             (when (debug-p (or :thot :http))
               (format t "~&~S -> ~S~%" handler-form handler))
             (call handler)
             (flush (reply-stream% reply))
             (return)))))
    (if (string-equal "keep-alive" (request-header 'connection))
        :keep-alive
        nil)))

(defvar *stop* nil)

(defun request-loop (request-stream reply-stream)
  (loop
     (when *stop*
       (return))
     (let* ((request (make-instance 'request :stream request-stream))
            (reply (make-instance 'reply :stream reply-stream))
            (result (funcall (request-reader request reply #'request-cont))))
       (unless (eq :keep-alive result)
         (return)))))

(defvar *acceptor-loop*)

(defvar *host*)

(defvar *port*)

(defun start (&key (host "0.0.0.0") (port 8000))
  (setq *stop* nil)
  (let ((*host* host)
        (*port* port))
    (socket:with-socket (fd socket:+af-inet+
                            socket:+sock-stream+
                            0)
      (socket:bind-inet fd host port)
      (socket:listen fd 128)
      (funcall (funcall *acceptor-loop* fd)))))

(defun set-nonblocking (fd)
  (let ((flags (fcntl:getfl fd)))
    (fcntl:setfl fd (logior fcntl:+o-nonblock+ flags))))
