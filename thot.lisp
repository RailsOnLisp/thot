;;
;;  Thot - http web server
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com> 0614550127
;;

(in-package :thot)

(setf (debug-p :thot) t)
(setf (debug-p :directory) t)
(setf (debug-p :file) t)
(setf (debug-p :mime) t)

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
   (remote-addr :initarg :remote-addr
                :accessor request-remote-addr%)
   (method :initarg :method
           :accessor request-method%
           :type (or null symbol))
   (target :initarg :target
           :accessor request-target%
           :type (or null string))
   (http-version :initarg :http-version
                 :accessor request-http-version%
                 :type (or null string))
   (headers :initform (make-hash-table :test 'equalp :size 32)
            :reader request-headers%
            :type hash-table)
   (data :initform nil
         :accessor request-data%
         :type (or null string))
   (uri :accessor request-uri%
        :type (or null string))
   (scheme :type (or null string))
   (host :type (or null string))
   (dir :type (or null string))
   (query :accessor request-query%
          :type (or null string))))

(defvar *request*)

(defun reset-request (&optional (request *request*))
  (declare (type request request))
  (setf (request-method% request) nil
        (request-target% request) nil
        (request-http-version% request) nil
        (request-data% request) nil
        (request-uri% request) nil
        (request-query% request) nil)
  (clrhash (request-headers% request))
  request)

(defun request-stream (&optional (request *request*))
  (declare (type request request))
  (request-stream% request))

(defun request-remote-addr (&optional (request *request*))
  (declare (type request request))
  (request-remote-addr% request))

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
  (gethash (string-downcase header-name)
           (request-headers% request)))

(defsetf request-header (header-name &optional (request '*request*))
    (value)
  `(setf (gethash (string-downcase ,header-name)
                  (request-headers% ,request)) ,value))

(defun request-content-length (&optional (request *request*))
  (let ((header (request-header "content-length" request)))
    (when header
      (parse-integer header))))

(defun request-data (&optional (request *request*))
  (declare (type request request))
  (request-data% request))

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

(defun request-reader (request reply)
  "Returns a HTTP request reader function which itself returns either :
 :EOF if end of stream was reached, or
 a closure if read would block, or
 the return value of CONT, which should be either :
  :KEEP-ALIVE if the connection is to be kept alive for next request, or
  NIL if the request stream is to be closed after reply."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((stream (request-stream request))
        (buffer (string-output-stream))
        (name "")
        (value "")
        (length nil))
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
                    (msg info (request-method% request) " "
                         (request-target% request) " "
                         (request-http-version% request))
                    (next-header))
                   (t (error "Missing request line LF"))))
           (next-header (char)
             (cond ((char= #\Return char) (end-of-headers))
                   (t (stream-write buffer char)
                      (header-name))))
           (header-name (char)
             (cond ((char= #\: char)
                    (setq name (string-downcase (get-buffer)))
                    (header-spaces))
                   (t (stream-write buffer char)
                      (header-name))))
           (header-spaces (char)
             (cond ((char= #\Space char) (header-spaces))
                   (t (stream-write buffer char)
                      (header-value))))
           (header-value (char)
             (cond ((char= #\Return char)
                    (setf value (get-buffer))
                    (header-lf))
                   (t (stream-write buffer char)
                      (header-value))))
           (header-lf (char)
             (unless (char= #\Newline char)
               (error "Missing header LF"))
             (when (debug-p (or :thot :http))
               (msg debug name " <- " value))
             (setf (request-header name request) value
                   value "")
             (next-header))
           (end-of-headers (char)
             (unless (char= #\Newline char)
               (error "Missing end of headers LF"))
             (setf length (request-content-length request))
             (if (and length (< 0 length))
                 (data)
                 (request-handler request reply)))
           (data (char)
                 (stream-write buffer char)
                 (let* ((char-string (make-string
                                      1 :initial-element char))
                        (char-length (trivial-utf-8:utf-8-byte-length
                                      char-string)))
                   (cond ((= 0 (decf length char-length))
                          (setf (request-data% request) (get-buffer))
                          (request-handler request reply))
                         (t
                          (data))))))
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
           :type output-stream)))

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
  (setf (reply-status) line))

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
           (pop headers)))))

(defun end-headers ()
  (unless (reply-headers-sent)
    (setf (reply-headers-sent) t)
    (header "")
    (let ((stream (reply-stream)))
      (stream-write-sequence stream (request-http-version))
      (stream-write stream #\Space)
      (stream-write-sequence stream (reply-status))
      (stream-write-sequence stream +crlf+)
      (dolist (header (reply-headers))
        (stream-write-sequence stream header)
        (stream-write-sequence stream +crlf+)))))

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

(defun probe-file (path)
  (when (with-stat (stat nil) path
          (s-isreg (the fixnum (stat-mode stat))))
    (= 0 (the fixnum (unistd:c-access path unistd:+r-ok+)))))

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
          (msg directory "name " name " url " url))
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
      (msg directory "dir " dir " local " local " remote " remote))
    (when (prefix-p remote dir)
      (let* ((subdir (subseq dir (length remote)))
             (local-path (str local subdir)))
        (when (debug-p :directory)
          (msg directory "subdir " subdir " local-path " local-path))
        (when (probe-dir local-path)
          `(directory-index ,local ,remote ,subdir))))))

(defun fd-file-size (fd)
  (with-fstat (stat) fd
    (stat-size stat)))

(defun stream-file-size (stream)
  (fd-file-size (stream-fd stream)))

(defun path-name (path)
  (declare (type simple-string path))
  (let ((start (position #\/ path :from-end t)))
    (if start
        (subseq path (1+ start))
        path)))

(defun path-extension (path)
  (declare (type simple-string path))
  (let* ((name (the simple-string (path-name path)))
         (start (position #\. name :from-end t)))
    (when start
      (ext (subseq name start)))))

(defun file (path)
  (let* ((ext (path-extension path))
         (type (mime-type ext)))
    (when (debug-p :file)
      (msg debug "path " path " ext " ext " type " type))
    (header "Content-Type: " (string-downcase (symbol-name type)))
    (with-stream (in (unistd-stream-open path :read t))
      (let ((size (the integer (stream-file-size in))))
        (header "Content-Length: " size)
        (end-headers)
        (unless (= 0 size)
          (stream-copy in (reply-stream)))))))

(defparameter *index-files*
  '("index.html"))

(defun file-handler (local remote)
  (declare (type simple-string local remote))
  (let ((uri (request-uri)))
    (declare (type simple-string uri))
    (when (debug-p :file)
      (msg file "uri " uri " local " local " remote " remote))
    (when (prefix-p remote uri)
      (let* ((path (subseq uri (length remote)))
             (local-path (str local path)))
        (when (debug-p :file)
          (msg file "local-path " local-path))
        (with-stat (stat nil) local-path
          (let ((mode (the fixnum (stat-mode stat))))
            (cond ((s-isdir mode)
                   (dolist (index *index-files*)
                     (let ((index-path (str local-path "/" index)))
                       (when (probe-file index-path)
                         (return `(file ,index-path))))))
                  ((s-isreg mode)
                   (when (= 0 (the fixnum
                                   (unistd:c-access local-path
                                                    unistd:+r-ok+)))
                     `(file ,local-path))))))))))

(defparameter *url-handlers*
  '((file-handler "/var/www/htdocs/" "/")
    (directory-handler "/var/www/htdocs/" "/")
    (404-not-found-handler)))

(defun call-handler-form (list)
  (apply (fdefinition (first list)) (rest list)))

(defun call-handler (list)
  (apply (fdefinition (first list)) (rest list)))

(defun request-handler (request reply)
  (let ((handlers *url-handlers*)
        (*request* request)
        (*reply* reply))
    (with-simple-restart (continue "Continue")
      (handler-bind
          ((cffi-errno:errno-error
            (lambda (condition)
              (let ((errno (the fixnum
                                (errno:errno-error-errno condition))))
                (when (find errno '(errno:+epipe+
                                    errno:+econnreset+))
                  (when (debug-p :thot)
                    (msg warn "request-handler: " condition))
                  (return-from request-handler))))))
        (loop
           (let ((handler-form (pop handlers)))
             (unless handler-form (return))
             (let ((handler (call-handler-form handler-form)))
               (when (debug-p :thot)
                 (format t "~A ~S -> ~S~%" 'debug handler-form handler)
                 (force-output))
               (when handler
                 (call-handler handler)
                 (stream-flush-output (reply-stream% reply))
                 (return)))))
        (if (string-equal "keep-alive" (request-header 'connection))
            :keep-alive
            nil)))
    nil))

(defvar *acceptor-loop*)
(declaim (type function *acceptor-loop*))

(defun main-loop (fd)
  (msg info " " *acceptor-loop*)
  (funcall *acceptor-loop* fd))

(defvar *stop* nil)

(defvar *main-loop* #'main-loop)
(declaim (type function *main-loop*))

(defvar *host*)

(defvar *port*)

(defun start (&key (host "0.0.0.0") (port 8000))
  (msg info "Thot start " host ":" port)
  (setq *stop* nil)
  (let ((*host* host)
        (*port* port))
    (configure-mime)
    (socket:with-socket (fd socket:+af-inet+
                            socket:+sock-stream+
                            0)
      (socket:bind-inet fd host port)
      (socket:listen fd 128)
      (msg info *main-loop*)
      (funcall *main-loop* fd))))

(defun set-nonblocking (fd)
  (let ((flags (the fixnum (fcntl:getfl fd))))
    (fcntl:setfl fd (logior fcntl:+o-nonblock+ flags))))

;(trace socket:socket socket:bind socket:bind-inet unistd:close unistd:c-close)
;(trace header content)
;(trace request-handler file-handler directory-handler call-handler-form call-handler)
;(untrace file-handler c-stat)
