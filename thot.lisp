;;
;;  Thot - http web server
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com> 0614550127
;;

(in-package :thot)

;;(setf (debug-p :thot) t)
;;(setf (debug-p :directory) t)
;;(setf (debug-p :file) t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+crlf+)
    (defconstant +crlf+
      (coerce '(#\Return #\Newline) 'string))))

;;  Methods

(let ((request-methods '(get post put head)))
  (defun http-method (x)
    (find x request-methods :test #'string=)))

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
  (let* ((target (the simple-string (request-target% request)))
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
  (declare (type simple-string uri))
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
  (declare (type simple-string uri))
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
  (declare (type simple-string uri))
  (let ((dir-end (position #\/ uri :from-end t)))
    (subseq uri 0 (or dir-end 0))))

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
                  (multiple-value-bind (,element ,state)
                      (stream-read ,stream)
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
             (prog1 (sequence-output-stream-sequence buffer)
               (sequence-output-stream-reset buffer))))
      (with-readers-for stream
          ((method (char)
             (cond ((char= #\Space char)
                    (setf (request-method% request)
                          (http-method (get-buffer)))
                    (target))
                   (t (stream-write buffer char)
                      (method))))
           (target (char)
             (cond ((char= #\Space char)
                    (setf (request-target% request) (get-buffer))
                    (version))
                   (t (stream-write buffer char)
                      (target))))
           (version (char)
             (cond ((char= #\Return char)
                    (setf (request-http-version% request) (get-buffer))
                    (version-lf))
                   (t (stream-write buffer char)
                      (version))))
           (version-lf (char)
             (cond ((char= #\Newline char)
                    (when (debug-p (or :thot :http))
                      (format t "~&thot: ~A ~A ~A~%"
                              (request-method% request)
                              (request-target% request)
                              (request-http-version% request))
                      (force-output))
                    (next-header))
                   (t (error "Missing request line LF"))))
           (next-header (char)
             (cond ((char= #\Return char) (end-of-headers))
                   (t (stream-write buffer char)
                      (header-name))))
           (header-name (char)
             (cond ((char= #\: char)
                    (setq name (get-buffer))
                    (header-spaces))
                   (t (stream-write buffer char)
                      (header-name))))
           (header-spaces (char)
             (cond ((char= #\Space char) (header-spaces))
                   (t (stream-write buffer char)
                      (header-value))))
           (header-value (char)
             (cond ((char= #\Return char)
                    (setq value (get-buffer))
                    (header-lf))
                   (t (stream-write buffer char)
                      (header-value))))
           (header-lf (char)
             (cond ((char= #\Newline char)
                    (when (debug-p (or :thot :http))
                      (format t "~&thot: ~A: ~A~%" name value)
                      (force-output))
                    (setf (request-header name request) value)
                    (next-header))
                   (t (error "Missing header LF"))))
           (end-of-headers (char)
             (cond ((char= #\Newline char)
                    (funcall cont request reply))
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
    (stream-write-sequence stream (request-http-version))
    (stream-write stream #\Space)
    (stream-write-sequence stream line)
    (stream-write-sequence stream +crlf+)))

(defun header (&rest parts)
  (unless (reply-status)
    (status "200 OK"))
  (let ((line (str parts))
        (headers (reply-headers)))
    (if (endp headers)
        (setf (reply-headers) (list line))
        (loop
           (when (endp (rest headers))
             (setf (rest headers) (list line))
             (return))
           (pop headers)))
    (let ((stream (reply-stream)))
      (stream-write-sequence stream line)
      (stream-write-sequence stream +crlf+))))

(defun end-headers ()
  (unless (reply-headers-sent)
    (setf (reply-headers-sent) t)
    (header "")))

(defun content (&rest parts)
  (end-headers)
  (let ((stream (reply-stream)))
    (walk-str (lambda (x)
                (stream-write-sequence stream x))
              parts)))

(defun 404-not-found ()
  (status "404 Not found")
  (header "Content-Type: text/plain")
  (content "404 Not found

The requested url "
           (request-target)
           " was not found on this server."))

(defun 404-not-found-handler ()
  '(404-not-found))

(defun h (string)
  (expand-entities string))

(defun url-encode (string)
  (rol-uri:%-encode string))

(defun path-directory-p (path)
  (declare (type simple-string path))
  (let ((len (length path)))
    (when (< 0 len)
      (char= #\/ (char path (1- len))))))

;;(path-directory-p "/")

(defun path-as-directory (path)
  (if (path-directory-p path)
      path
      (str path "/")))

;;(path-as-directory "/")

(defun probe-dir (path)
  (let ((dirp (dirent:c-opendir path)))
    (unless (cffi:null-pointer-p dirp)
      (dirent:closedir dirp)
      t)))

(defun prefix-p (pre str)
  (declare (type simple-string pre str))
  (and (<= (length pre) (length str))
       (string= pre str :end2 (length pre))))

(defun sorted-dir (path)
  (let ((dirs)
        (files))
    (do-dir (df path)
      (if (= +dt-dir+ (the fixnum (dirent-type df)))
          (push (str (dirent-name df) "/") dirs)
          (push (dirent-name df) files)))
    (append (sort dirs #'string<)
            (sort files #'string<))))

(defun directory-index (local remote dir)
  (header "Content-Type: text/html")
  (content "<html>
 <head>
  <title>" (h remote) (h dir) "</title>
 </head>
 <body>
  <h1>" (h remote) (h dir) "</h1>
  <ul>
")
  (let* ((localdir (str local dir))
         (sorted (sorted-dir localdir)))
    (dolist (name sorted)
      (let* ((url (str remote dir (url-encode name))))
        (when (debug-p :directory)
          (format t "name ~S url ~S~%" name url)
          (force-output))
        (content "   <li><a href=\"" (h url) "\">"
                 (h name)
                 "</a></li>
"))))
  (content "  </ul>
 </body>
</html>
"))

(defun directory-handler (local remote)
  (let ((dir (path-as-directory (request-uri))))
    (when (debug-p :directory)
      (format t "dir ~S local ~S remote ~S~%" dir local remote)
      (force-output))
    (when (prefix-p remote dir)
      (let* ((subdir (subseq dir (length remote)))
             (local-path (str local subdir)))
        (when (debug-p :directory)
          (format t "subdir ~S local-path ~S~%" subdir local-path)
          (force-output))
        (with-stat (stat nil) local-path
          (when (s-isdir (stat-mode stat))
            `(directory-index ,local ,remote ,subdir)))))))

(defun fd-file-size (fd)
  (let ((end (unistd:lseek fd 0 unistd:+seek-end+)))
    (unistd:lseek fd 0 unistd:+seek-set+)
    end))

(defun stream-file-size (stream)
  (fd-file-size (stream-fd stream)))

(defun path-name (path)
  (declare (type simple-string path))
  (let ((start (position #\/ path :from-end t)))
    (when start
      (subseq path (1+ start)))))

(defun path-extension (path)
  (declare (type simple-string path))
  (let* ((name (the simple-string (path-name path)))
         (start (position #\. name :from-end t)))
    (when start
      (ext (subseq  path start)))))

(defun file (path)
  (let* ((ext (path-extension path))
         (type (mime-type ext)))
    (header "Content-Type: " type)
    (with-stream (in (unistd-stream-open path :read t))
      (let ((size (stream-file-size in)))
        (header "Content-Length: " size))
      (end-headers)
      (stream-copy in (reply-stream)))))

(defun file-handler (local remote)
  (declare (type simple-string local remote))
  (let ((uri (request-uri)))
    (declare (type simple-string uri))
    (when (debug-p :file)
      (format t "uri ~S local ~S remote ~S~%" uri local remote)
      (force-output))
    (when (prefix-p remote uri)
      (let* ((path (subseq uri (length remote)))
             (local-path (str local path)))
        (when (debug-p :file)
          (format t "local-path ~S~%" local-path)
          (force-output))
        (with-stat (stat nil) local-path
          (when (s-isreg (stat-mode stat))
            `(file ,local-path)))))))

(defparameter *url-handlers*
  '((file-handler "/var/www/htdocs/" "/")
    (directory-handler "/var/www/htdocs/" "/")
    (404-not-found-handler)))

(defun call-handler-form (list)
  (apply (first list) (rest list)))

(defun call-handler (list)
  (apply (first list) (rest list)))

(defun request-cont (request reply)
  (let ((handlers *url-handlers*)
        (*request* request)
        (*reply* reply))
    (handler-bind
        ((errno:errno-error (lambda (condition)
                              (cond ((= errno:+epipe+
                                        (errno:errno-error-errno condition))
                                     (format t "~&~S~%" condition)
                                     (return-from request-cont))))))
      (loop
         (let ((handler-form (pop handlers)))
           (let ((handler (call-handler-form handler-form)))
             (when (debug-p :thot)
               (format t "~&~S -> ~S~%" handler-form handler)
               (force-output))
             (when handler
               (call-handler handler)
               (stream-flush (reply-stream% reply))
               (return)))))
      (if (string-equal "keep-alive" (request-header 'connection))
          :keep-alive
          nil))))

(defvar *stop* nil)

(defun request-loop (request-stream reply-stream)
  (loop
     (when *stop*
       (return))
     (handler-case
         (let* ((req (make-instance 'request :stream request-stream))
                (reply (make-instance 'reply :stream reply-stream))
                (reader (request-reader req reply #'request-cont))
                (result (funcall reader)))
           (unless (eq :keep-alive result)
             (return)))
       (warning (w)
         (format t "WARN ~A" w)
         (force-output)
         (continue)))))

(defvar *acceptor-loop*)

(defvar *host*)

(defvar *port*)

(defun start (&key (host "0.0.0.0") (port 8000))
  (when (debug-p :thot)
    (format t "~&Thot start ~A ~A~%" host port)
    (force-output))
  (setq *stop* nil)
  (let ((*host* host)
        (*port* port))
    (socket:with-socket (fd socket:+af-inet+
                            socket:+sock-stream+
                            0)
      (socket:bind-inet fd host port)
      (socket:listen fd 128)
      (when (debug-p :thot)
        (format t "~A~%" *acceptor-loop*)
        (force-output))
      (funcall (funcall *acceptor-loop* fd)))))

(defun set-nonblocking (fd)
  (let ((flags (the fixnum (fcntl:getfl fd))))
    (fcntl:setfl fd (logior fcntl:+o-nonblock+ flags))))

;(trace socket:socket socket:bind socket:bind-inet unistd:close unistd:c-close)
;(trace header content)
;(trace request-cont file-handler directory-handler call-handler-form call-handler)
