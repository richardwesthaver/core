;;; http.lisp --- HTTP Services

;; HTTP/S Service (based on Hunchentoot)

;;; Commentary:

;; This module contains the main HTTP/S web application server machinery for
;; core modules. Loading this file should give you the basics needed to build
;; a CLOS-based asynchronous web server.

;;; Code:
(in-package :net/srv/http)

(defvar *headers-sent* nil
  "Used internally to check whether the response headers have
already been sent for this request.")
(defvar *service-header-stream* nil)
(defvar *default-ssl-service-port* 8443)
(defvar *default-content-type* "text/html")
(defvar *default-ssl-key-file* #P"/etc/ssl/cert.pem")
(defvar *http-external-format* :default)
(defvar *header-stream* nil)
;; TODO 2025-04-11: 
(defvar *rewrite-for-session-urls* t
  "Whether HTML pages should possibly be rewritten for cookie-less
session-management.")
(defvar *content-types-for-url-rewrite*
  '("text/html" "application/xhtml+xml")
  "The content types for which url-rewriting is OK. See
*REWRITE-FOR-SESSION-URLS*.")

;;; Utils
(defun keep-alive-p (&optional (object *request*))
  (typep (content-stream object) 'net/req::keep-alive-stream))

(defun ssl-p (&optional (service *service*))
  (and (secure-service-p service)
       (eql :https (sb-bsd-sockets:socket-protocol (sb-bsd-sockets:socket service)))))

;;; Config
(defconfig http-service-config (net-service-config) ())

(defmethod make-config ((self (eql :http)) &rest args &key)
  (apply 'make-instance 'http-service-config args))

;;; Server
(defclass http-server (tcp-server) ())

;;; Response
(defclass http-service-response (net-service-response)
  ((http :type http-response)
   (headers-out :initform nil
                :reader headers-out
                :documentation "An alist of the outgoing http headers
not including the 'Set-Cookie', 'Content-Length', and 'Content-Type'
headers.  Use the functions HEADER-OUT and \(SETF HEADER-OUT) to
modify this slot.")
   (cookies-out :initform nil
                :accessor cookies-out
                :documentation "The outgoing cookies.  This slot's
value should only be modified by the functions defined in cookie.lisp.")))

(defmethod content-length ((res http-service-response))
  (http-content-length (slot-value res 'response)))

(defmethod response-status ((res http-service-response))
  (http-status (slot-value *response* 'response)))

(defmethod (setf response-status) (new (res http-service-response))
  (setf (http-status (slot-value *response* 'response)) new))

(defmethod response-ok-p ((res http-service-response))
  (eql (response-status (slot-value *response* 'response)) codec::+http-ok+))

(defun header-in* (name &optional (req *request*))
  (header-out name req))

(defun headers-out* (&optional (res *response*))
  "Returns an alist of the outgoing headers associated with the
RESPONSE object."
  (headers-out res))

(defun cookies-out* (&optional (response *response*))
  "Returns an alist of the outgoing cookies associated with the
RESPONSE object."
  (cookies-out response))

(defun (setf cookies-out*) (new-value &optional (res *response*))
  "Sets the alist of the outgoing cookies associated with the RESPONSE
object RES."
  (setf (cookies-out res) new-value))

(defmethod content-type ((res http-service-response))
  (gethash "content-type" (http:http-headers res)))

(defun content-type* (&optional (res *response*))
  "The outgoing 'Content-Type' http header of RES."
  (content-type res))

(defun (setf content-type*) (new-value &optional (res *response*))
  "Sets the outgoing 'Content-Type' http header of RES."
  (setf (header-out :content-type res) new-value))

(defun content-length* (&optional (res *response*))
  "The outgoing 'Content-Length' http header of RES."
  (content-length res))

(defun (setf content-length*) (new-value &optional (res *response*))
  "Sets the outgoing 'Content-Length' http header of RES."
  (setf (header-out :content-length res) new-value))

(defun response-status* (&optional (res *response*))
  "The http return code of RES.  The return codes Hunchentoot can
handle are defined in specials.lisp."
  (response-status res))

(defun (setf response-status*) (new-value &optional (res *response*))
  "Sets the http return code of RES."
  (setf (response-status res) new-value))

(defun header-out-set-p (name &optional (res *response*))
  "Returns a true value if the outgoing http header named NAME has
been specified already.  NAME should be a keyword or a string."
  #|assoc*|# (assoc name (headers-out res)))

(defun header-out (name &optional (res *response*))
  "Returns the current value of the outgoing http header named NAME.
NAME should be a keyword or a string."
  (cdr (assoc name (headers-out res))))

(defun cookie-out (name &optional (res *response*))
  "Returns the current value of the outgoing cookie named
NAME. Search is case-sensitive."
  (cdr (assoc name (cookies-out res) :test #'string=)))

(defgeneric (setf header-out) (new-value name &optional res)
  (:documentation "Changes the current value of the outgoing http
header named NAME \(a keyword or a string).  If a header with this
name doesn't exist, it is created.")
  (:method (new-value (name symbol) &optional (res *response*))
    ;; the default method
    (let ((entry (assoc name (headers-out res))))
      (if entry
          (setf (cdr entry) new-value)
          (setf (slot-value res 'headers-out)
                (acons name new-value (headers-out res))))
      new-value))
  (:method (new-value (name string) &optional (res *response*))
    "If NAME is a string, it is converted to a keyword first."
    (setf (header-out (keywordicate name) res) new-value))
  (:method :after (new-value (name (eql :content-length)) &optional (res *response*))
    "Special case for the `Content-Length' header."
    (check-type new-value integer)
    (setf (slot-value res 'content-length) new-value))
  (:method :after (new-value (name (eql :content-type)) &optional (res *response*))
    "Special case for the `Content-Type' header."
    (check-type new-value (or null string))
    (setf (slot-value res 'content-type) new-value)))


(let ((scanner-hash (make-hash-table :test #'equal)))
  (defun scanner-for-get-param (param-name)
    "Returns a CL-PPCRE scanner which matches a GET parameter in a
URL.  Scanners are memoized in SCANNER-HASH once they are created."
    (or (gethash param-name scanner-hash)
        (setf (gethash param-name scanner-hash)
              (cl-ppcre:create-scanner
               `(:alternation
                 ;; session=value at end of URL
                 (:sequence
                  (:char-class #\? #\&)
                  ,param-name
                  #\=
                  (:greedy-repetition 0 nil (:inverted-char-class #\&))
                  :end-anchor)
                 ;; session=value with other parameters following
                 (:sequence
                  (:register (:char-class #\? #\&))
                  ,param-name
                  #\=
                  (:greedy-repetition 0 nil (:inverted-char-class #\&))
                  #\&))))))
  (defun add-cookie-value-to-url (url &key
                                      (cookie-name (session-cookie-name *service*))
                                      (value (when-let ((session (session *request*)))
                                               (session-cookie-value session)))
                                      (replace-ampersands-p t))
    "Removes all GET parameters named COOKIE-NAME from URL and then
adds a new GET parameter with the name COOKIE-NAME and the value
VALUE.  If REPLACE-AMPERSANDS-P is true all literal ampersands in URL
are replaced with '&amp;'. The resulting URL is returned."
    (unless url
      ;; see URL-REWRITE:*URL-REWRITE-FILL-TAGS*
      (setq url (uri:uri *request*)))
    (setq url (cl-ppcre:regex-replace-all (scanner-for-get-param cookie-name) url "\\1"))
    (when value
      (setq url (format nil "~A~:[?~;&~]~A=~A"
                        url
                        (find #\? url)
                        cookie-name
                        (url:url-encode value))))
    (when replace-ampersands-p
      (setq url (cl-ppcre:regex-replace-all "&" url "&amp;")))
    url))

(defun maybe-rewrite-urls-for-session (html &key
                                            (cookie-name (session-cookie-name *service*))
                                            (value (when-let ((session (session *request*)))
                                                     (session-cookie-value session))))
  "Rewrites the HTML page HTML such that the name/value pair
COOKIE-NAME/COOKIE-VALUE is inserted if the client hasn't sent a
cookie of the same name but only if *REWRITE-FOR-SESSION-URLS* is
true.  See the docs for URL-REWRITE:REWRITE-URLS."
  (cond ((or (not *rewrite-for-session-urls*)
             (null value)
             (cookie-out cookie-name))
         html)
        (t
         (with-input-from-string (*standard-input* html)
           (with-output-to-string (*standard-output*)
             (url:rewrite-urls
              (lambda (url)
                (add-cookie-value-to-url url
                                         :cookie-name cookie-name
                                         :value value))))))))

(defun maybe-add-charset-to-content-type-header (content-type external-format)
  (if (and (cl-ppcre:scan "(?i)^text" content-type)
           (not (cl-ppcre:scan "(?i);\\s*charset=" content-type)))
      (format nil "~A; charset=~(~A~)" content-type external-format)
      content-type))

;; headers-out
;; return-code * status-code
;; external-format //
;; cookies-out

;;; Request

;; method
;; uri
;; headers-in
;; cookies-in
;; get-parameters
;; post-parameters
;; script-name
;; query-string
;; raw-post-data

(defclass http-service-request (net-service-request)
  ((http :type http-request :initarg :http :accessor http)
   (headers-in :initarg :headers-in :reader headers-in)
   (cookies-in :initform nil
               :documentation "cookies sent by the client."
               :reader cookies-in)
   (get-parameters :initform nil
                   :documentation "An alist of the GET parameters sent
by the client."
                   :reader get-parameters)
   (post-parameters :initform nil
                    :documentation "An alist of the POST parameters
sent by the client."
                    :reader post-parameters)
   (script-name :initform nil
                :documentation "The URI requested by the client without
the query string."
                :reader script-name)
   (query-string :initform nil
                 :documentation "The query string of this request."
                 :reader query-string)
   (session :initform nil
            :accessor session
            :documentation "The session object associated with this
request.")
   (aux-data :initform nil
             :accessor aux-data
             :documentation "Used to keep a user-modifiable alist with
arbitrary data during the request.")))

(defun request-method* (&optional (req *request*))
  (http-method (http req)))

(defun script-name* (&optional (req *request*))
  (script-name req))

(defun query-string* (&optional (req *request*))
  (query-string req))

(defun user-agent* (&optional (req *request*))
  (user-agent (session req)))

(defun referer* (&optional (req *request*))
  (header-in* :referer req))

(defun authorization* (&optional (req *request*))
  (header-in* :authorization req))

(defun cookie-in (name &optional (req *request*))
  "Returns the current value of the incoming cookie named
NAME. Search is case-sensitive."
  (cdr (assoc name (cookies-in req) :test #'string=)))

(defun start-http-output (status &optional (content nil contentp))
  "Sends all headers and maybe the content body to *SERVICE-STREAM*. Returns
immediately and does nothing if called more than once per request. Called by
PROCESS-REQUEST and/or SEND-HEADERS. The STATUS argument represents the
integer return code of the request. The corresponding reason phrase is
determined by calling the HTTP-STATUS-MESSAGE function. The CONTENT provided
represents the body data to send to the client, if any. If it is not
specified, no body is written to the client. The handler function is expected
to directly write to the stream in this case.

Returns the stream that is connected to the client."
  (let* ((chunkedp (and (output-chunking-p *service*)
                        (eq (request-protocol *request*) :http/1.1)
                        ;; only turn chunking on if the content
                        ;; length is unknown at this point...
                        (null (or (content-length*) contentp))))
         (request-method (http-method (http *request*)))
         (head-request-p (eq request-method :head))
         content-modified-p)
    (multiple-value-bind (keep-alive-p keep-alive-requested-p)
        (keep-alive-p *request*)
      (when keep-alive-p
        (setq keep-alive-p
              ;; use keep-alive if there's a way for the client to
              ;; determine when all content is sent (or if there
              ;; is no content)
              (or chunkedp
                  head-request-p
                  (eql (response-status*) codec::+http-not-modified+)
                  (content-length*)
                  content)))
      ;; now set headers for keep-alive and chunking
      (when chunkedp
        (setf (header-out :transfer-encoding) "chunked"))
      (cond (keep-alive-p
             (setf *finish-processing-socket* nil)
             ;; read-timeout
             (when (and (service-timeout *service*)
                        (or (not (eq (request-protocol *request*) :http/1.1))
                            keep-alive-requested-p))
               ;; persistent connections are implicitly assumed for
               ;; HTTP/1.1, but we return a 'Keep-Alive' header if the
               ;; client has explicitly asked for one
               (unless (header-out :connection) ; allowing for handler overriding
                 (setf (header-out :connection) "Keep-Alive"))
               (setf (header-out :keep-alive)
                     ;; read-timeout
                     (format nil "timeout=~D" (service-timeout *service*)))))
            ((not (header-out-set-p :connection))
             (setf (header-out :connection) "Close"))))
    (unless (and (header-out-set-p :server)
                 (null (header-out :server)))
      (setf (header-out :server) (or (header-out :server)
                                     (name *service*))))
    (setf (header-out :date) (time:rfc-1123-date))
    (when (and (stringp content)
               (not content-modified-p)
               (starts-with-one-of-p (or (content-type*) "")
                                     *content-types-for-url-rewrite*))
      ;; TODO 2025-07-24: 
      ;; if the Content-Type header starts with one of the strings
      ;; in *CONTENT-TYPES-FOR-URL-REWRITE* then maybe rewrite the
      ;; content
      ;; (setq content (maybe-rewrite-urls-for-session content))
      )
    (when (stringp content)
      ;; if the content is a string, convert it to the proper external format
      (setf content (sb-ext:string-to-octets content :external-format *http-external-format*)
            (content-type*) (net/req::charset-to-encoding (content-type*)
                                                          *http-external-format*)))
    (when content
      ;; whenever we know what we're going to send out as content, set
      ;; the Content-Length header properly; maybe the user specified
      ;; a different content length, but that will wrong anyway
      (setf (header-out :content-length) (length content)))
    ;; send headers only once
    (when *headers-sent*
      (return-from start-http-output))
    (setq *headers-sent* t)
    (send-http-response *service*
                        *service-stream*
                        status
                        :headers (headers-out*)
                        :cookies (cookies-out*)
                        :content (unless head-request-p
                                   content))
    ;; when processing a HEAD request, exit to return from PROCESS-REQUEST
    (when head-request-p
      (throw 'request-processed nil))
    (when chunkedp
      ;; turn chunking on after the headers have been sent
      (unless (typep *service-stream* 'chunked-stream)
        (setq *service-stream* (make-chunked-stream *service-stream*)))
      (setf (output-chunking-p *service-stream*) t))
    *service-stream*))

(defun send-http-headers ()
  (start-http-output (response-status*)))

(defmethod process-request ((req http-service-request))
  (catch 'request-processed ;; used by HTTP HEAD handling to end request
    ;; processing in a HEAD request (see START-HTTP-OUTPUT)
    (let ((*request* req)
          ;; *tmp-files*
          *headers-sent*)
      (labels
          ((report-error-to-client (error &optional backtrace)
             (when *log-service-errors*
               (service-log log:*log-level* "~A~@[~%~A~]" error (when log:*log-show-backtrace*
                                                                  backtrace)))
             (start-http-output codec::+http-internal-server-error+
                                (service-status-message 
                                 *service*
                                 codec::+http-internal-server-error+
                                 :error (princ-to-string error)
                                 :backtrace (princ-to-string backtrace)))))
        (multiple-value-bind (contents error backtrace)
            ;; skip dispatch if bad request
            (when (response-ok-p *response*)
              (catch 'handler-done
                (values (handle-request *service* *request*))))
          (declare (ignorable error backtrace))
          (when error
            ;; error occurred in request handler
            (report-error-to-client error backtrace))
          (unless *headers-sent*
            (start-http-output (response-status *response*)
                               (or contents
                                   (service-status-message 
                                    *service*
                                    (response-status *response*))))))))))

;;; Session
(defclass http-session (session)
  ((id :type integer :initarg :id)
   (user-agent :reader user-agent :initarg :user-agent)
   (remote-addr :reader remote-addr :initarg :remote-addr)
   (last-click :reader last-click :initarg :last-click))
  (:default-initargs
   :last-click (get-universal-time)))

(defvar *session-encode-user-agent* t)
(defvar *session-encode-remote-addr* nil)

(defun encode-session-string (id &optional user-agent remote-addr (start 0))
  (unless (boundp '*session-secret*)
    (simple-service-warning "Session secret is unbound.  Using Lisp's RANDOM function to initialize it.")
    (reset-session-secret))
  ;; *SESSION-SECRET* is used twice due to known theoretical
  ;; vulnerabilities of MD5 encoding
  (sb-md5:md5sum-string 
   (concatenate 'string
                *session-secret*
                (sb-md5:md5sum-string 
                 (format nil "~A~A~@[~A~]~@[~A~]~A"
                         *session-secret*
                         id
                         (and *session-encode-user-agent*
                              user-agent)
                         (and *session-encode-remote-addr*
                              remote-addr)
                         start)))))

(defun stringify-session (session)
  "Create a string representation of a SESSION object."
  (encode-session-string (id:id session)
                         (user-agent session)
                         (remote-addr session)
                         (start-session session)))

(defmethod session-expired-p ((self http-session))
  (< (+ (last-click self) (session-timeout self))
     (get-universal-time)))

(defun get-session (id)
  (let ((session
          (cdr (assoc id (net/srv::session-db *service*) :test #'=))))
    (when (and session
               (session-expired-p session))
      (when *response*
        (service-log :info "Session with ID ~A too old" id))
      (remove-session session)
      (setq session nil))
    session))

(defun set-cookie* (cookie &optional (res *response*))
  "Adds the COOKIE object COOKIE to the outgoing cookies of the
RESPONSE object. If a cookie with the same name
\(case-sensitive) already exists, it is replaced."
  (let* ((name (cookie-name cookie))
         (place (assoc name (cookies-out res) :test #'string=)))
    (cond
      (place
       (setf (cdr place) cookie))
      (t
       (push (cons name cookie) (cookies-out res))
       cookie))))

(defgeneric session-cookie-value (session)
  (:method ((session session))
    (and session
         (format nil
                 "~D:~A"
                 (id:id session)
                 (stringify-session session)))))

(defgeneric session-cookie-name (session)
  (:method ((session session))
    "srv-session"))

(defun refresh-session-cookie-value (session)
  (setf (slot-value session 'session-start) (get-universal-time)
        (slot-value session 'session-string) (stringify-session session))
  (set-cookie* (make-cookie :name (session-cookie-name *service*)
                            :value (session-cookie-value session)
                            :path "/"
                            :httponly-p t)))

(defun html-session-hook ()
  (set-cookie* (make-cookie :name (session-cookie-name *session*)
                            :value (session-cookie-value *session*)
                            :path "/"
                            :httponly-p t)))

;;; Service
(defclass http-service (net-service http-server) 
  ;; RESEARCH 2024-07-18: 
  ;; may need to start dealing with this
  ;; https://datatracker.ietf.org/doc/html/rfc2616#section-3.6.1
  ((connection-max :type (or fixnum null) :initarg :connection-max)
   (chunk-output-p :type boolean :initarg :chunk-output-p)
   (chunk-input-p :type boolean :initarg :chunk-input-p)
   (document-root :type pathname :initarg :document-root :accessor service-document-root))
  (:default-initargs
   :type :stream
   :protocol :tcp
   :engine (make-instance 'thread-per-connection-engine :name :http)
   :connection-max *default-connection-max*
   :request-class 'http-service-request
   :response-class 'http-service-response
   :chunk-output-p t
   :chunk-input-p t))

(defmethod reset-connection-stream ((self http-service) stream)
  (cond ((typep stream 'chunked-stream)
         (setf (output-chunking-p stream) nil
               (input-chunking-p stream) nil)
         (stream-of stream))
        (t stream)))

(defmethod service-log-access ((self http-service) &optional code)
  "Default method for access logging.  It logs the information to the
destination determined by (ACCESS-LOG-OUTPUT SERVICE) in a format that
can be parsed by most log analysis tools."
  (log:with-log-stream (stream (access-log-output self) *access-log-lock*)
    (format stream "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                    ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
            (remote-addr*)
            (header-in* :x-forwarded-for)
            (authorization*)
            (time:iso-time)
            (request-method*)
            (script-name*)
            (query-string*)
            (request-protocol*)
            code
            (content-length*)
            (referer*)
            (user-agent*))))

(defmethod handle-request ((*service* http-service) (*request* http-service-request))
  (handler-bind ((error
                   (lambda (c)
                     (when *headers-sent*
                       (setq *finish-processing-socket* t))
                     (throw 'handler-done
                       (values nil c (sb-debug:list-backtrace))))))
    (dispatch-request *service* *request*)))

;;; Default HTTP Handlers
(defun maybe-handle-range-header (file)
  "Helper function for handle-static-file.  Determines whether the
  requests specifies a Range header.  If so, parses the header and
  position the already opened file to the location specified.  Returns
  the number of bytes to transfer from the file.  Invalid specified
  ranges are reported to the client with a HTTP 416 status code."
  (let ((bytes-to-send (file-length file)))
    (cl-ppcre:register-groups-bind
        (start end)
        ("^bytes=(\\d+)-(\\d*)$" (header-in* :range) :sharedp t)
      ;; body won't be executed if regular expression does not match
      (setf start (parse-integer start))
      (setf end (if (> (length end) 0)
                    (parse-integer end)
                    (1- (file-length file))))
      (when (or (< start 0)
                (>= end (file-length file)))
        (setf (http-status *response*) codec::+http-requested-range-not-satisfiable+
              (header-out :content-range) (format nil "bytes 0-~D/~D" (1- (file-length file)) (file-length file)))
        (throw 'handler-done
          (format nil "invalid request range (requested ~D-~D, accepted 0-~D)"
                  start end (1- (file-length file)))))
      (file-position file start)
      (setf (http-status *response*) codec::+http-partial-content+
            bytes-to-send (1+ (- end start))
            (header-out :content-range) (format nil "bytes ~D-~D/~D" start end (file-length file))))
    bytes-to-send))

(defun handle-static-file (path &optional content-type callback)
  "An HTTP handler for the file PATH. Sends a CONTENT-TYPE header or tries to determine it from the file suffix.

CALLBACK is a function which is called just before sending the file to a
socket and can be used to set headers and check authorization. Arguments are
PATH and CONTENT-TYPE."
  (when (or (wild-pathname-p path)
            (not (uiop:file-exists-p path))
            (uiop:directory-exists-p path))
    ;; file doesn't exist
    (setf (http-status *response*) codec::+http-not-found+)
    (abort-request-handler))
  (unless content-type
    (setf content-type (dat/mime:get-mime path)))
  (let ((time (or (file-write-date path) (get-universal-time)))
        bytes)
    (setf (content-type*) (or (and content-type
                                   (maybe-add-charset-to-content-type-header content-type :utf-8))
                              "application/octet-stream")
          (header-out :last-modified) (time:date time)
          (header-out :accept-ranges) "bytes")
    ;; (handle-if-modified-since time)
    (unless (null callback)
      (funcall callback path content-type))
    (with-open-file (file path :direction :input :element-type 'octet)
      (setf bytes (maybe-handle-range-header file)
            (content-length*) bytes)
      (let ((out (send-http-headers))
            (buf (make-octets +default-chunked-output-size+)))
        (loop (when (zerop bytes) (return))
              (let ((chunk-size (min +default-chunked-output-size+ bytes)))
                (unless (eql chunk-size (read-sequence buf file :end chunk-size))
                  (error 'file-error :pathname file))
                (write-sequence buf out :end chunk-size)
                (decf bytes chunk-size)))
        (finish-output out)))))

;;; Dispatch
(defmethod dispatch-request ((service http-service) request)
  "Default implementation of the HTTP request dispatch method, generates an
+HTTP-NOT-FOUND+ error."
  (let ((path (and (service-document-root service)
                   ;; request-pathname?
                   (path request))))
    (cond
      (path
       (handle-static-file
        (merge-pathnames (if (equal "/" (script-name request)) #P"index.html" path)
                         (service-document-root service))))
      (t (setf (http-status *response*) codec::+http-not-found+)
         (abort-request-handler)))))

(defun get-post-data (&key (request *request*) want-stream (position 0))
  (let* ((headers-in (headers-in request))
         (content-length (when-let ((len (assoc :content-length headers-in
                                                :test 'eq)))
                           (parse-integer (car len) :junk-allowed t)))
         (content-stream (content-stream request)))
    (setf (slot-value request 'data)
          (cond (want-stream (io/stream:make-decoding-stream content-stream :external-format *http-external-format*))
                ((and content-length (> content-length position))
                 (decf content-length position)
                 (when (input-chunking-p *service-stream*)
                   ;; log-message
                   )
                 (let ((content (make-array content-length :element-type 'octet)))
                   (read-sequence content content-stream)
                   content))
                ((input-chunking-p *service-stream*)
		 (loop with buffer = (make-array net/req::+buffer-size+ :element-type 'octet)
		       with content = (make-array 0 :element-type 'octet :adjustable t)
		       for index = 0 then (+ index pos)
		       for pos = (read-sequence buffer content-stream)
		       do (adjust-array content (+ index pos))
			  (replace content buffer :start1 index :end2 pos)
		       while (= pos net/req::+buffer-size+)
		       finally (return content)))))))


(defun raw-post-data (&key (request *request*) want-stream force-binary force-string)
  (when (and force-binary force-string)
    (std-error "FORCE-BINARY and FORCE-STRING are mutually exclusive."))
  (let ((raw-post-data (or (slot-value request 'data)
                           (get-post-data :request request :want-stream want-stream))))
    (cond ((typep raw-post-data 'stream) raw-post-data)
          ((member raw-post-data '(t nil)) nil)
          (force-string (sb-ext:octets-to-string raw-post-data :external-format *http-external-format*))
          (t raw-post-data))))

(defun send-http-response (service stream status-code 
                           &key headers cookies content)
  "Send a HTTP response to STREAM and log it with SERVICE.

STATUS-CODE is the HTTP status code used in the response, HEADERS and COOKIES
are used to generate the header. If CONTENT is provided, it is used as the
body.

Headers are written to *HEADER-STREAM* when non-nil. 

Returns STREAM."
  (when content
    (setf (content-length*) (length content)))
  (when (content-length*)
    (if (assoc :content-length headers)
        (setf (cdr (assoc :content-length headers)) (content-length*))
        (push (cons :content-length (content-length*)) headers)))
  (service-log-access service status-code)
  (raw-post-data :force-binary t)
  ;; TODO flexi-stream with latin-1?
  (let* ((hstream stream))
    (format hstream "HTTP/1.1 ~D ~A~C~C" status-code (http-status-message status-code) #\Return #\Linefeed)
    (loop for (k . v) in headers
          when v
          do (req::write-header (string-upcase k) v hstream))
    ;; cookies
    (loop for (nil . cookie) in cookies
          do (req::write-header "Set-Cookie" (stringify-cookie cookie) hstream))
    (format hstream "~C~C" #\Return #\Linefeed))
  (when content
    (write-sequence content stream)
    (finish-output stream))
  stream)

(defun aux-request-value (symbol &optional (request *request*))
  "Returns the value associated with SYMBOL from the request object
REQUEST \(the default is the current request) if it exists.  The
second return value is true if such a value was found."
  (when request
    (let ((found (assoc symbol (aux-data request) :test #'eq)))
      (values (cdr found) found))))

(defsetf aux-request-value (symbol &optional request)
    (new-value)
  "Sets the value associated with SYMBOL from the request object
REQUEST \(default is *REQUEST*).  If there is already a value
associated with SYMBOL it will be replaced."
  (once-only (symbol)
    (with-gensyms (place %request)
      `(let* ((,%request (or ,request *request*))
              (,place (assoc ,symbol (aux-data ,%request) :test #'eq)))
         (cond
           (,place
            (setf (cdr ,place) ,new-value))
           (t
            (push (cons ,symbol ,new-value)
                  (aux-data ,%request))
            ,new-value))))))

(defun delete-aux-request-value (symbol &optional (request *request*))
  "Removes the value associated with SYMBOL from the request object
REQUEST."
  (when request
    (setf (aux-data request)
            (delete symbol (aux-data request)
                    :key #'car :test #'eq)))
  (values))

(defun parse-path (path)
  "Return a relative pathname that has been verified to not contain
  any directory traversals or explicit device or host fields.  Returns
  NIL if the path is not acceptable."
  (when (every #'graphic-char-p path)
    (let* ((pathname (sb-ext:parse-native-namestring
                      ;; Just disallow anything with :wild components later.
                      (remove #\\ (cl-ppcre:regex-replace "^/*" path ""))))
           (directory (pathname-directory pathname)))
      (when (and (or (null (pathname-host pathname))
                     (equal (pathname-host pathname)
                            (pathname-host *default-pathname-defaults*)))
                 (or (null (pathname-device pathname))
                     (equal (pathname-device pathname)
                            (pathname-device *default-pathname-defaults*)))
                 (or (null directory)
                     (and (eql (first directory) :relative)
                          ;; only string components, no :UP traversals or :WILD
                          (every #'stringp (rest directory))))
                 (not (equal (file-namestring pathname) "..")))
        pathname))))

(defun request-pathname (&optional (request *request*) drop-prefix)
  "Construct a relative pathname from the request's SCRIPT-NAME.
If DROP-PREFIX is given, pathname construction starts at the first path
segment after the prefix.
"
  (let ((path (script-name request)))
    (if drop-prefix
        (when (starts-with-p path drop-prefix)
          (parse-path (subseq path (length drop-prefix))))
        (parse-path path))))

(defun send-bad-request-response (stream &optional additional-info)
  "Send a ``Bad Request'' response to the client."
  (write-sequence (flex:string-to-octets
		   (format nil "HTTP/1.0 ~D ~A~C~CConnection: close~C~C~C~CYour request could not be interpreted by this HTTP server~C~C~@[~A~]~C~C"
			   codec::+http-bad-request+ (http-status-message codec::+http-bad-request+) #\Return #\Linefeed
			   #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed additional-info #\Return #\Linefeed))
		  stream))

(defconstant +http-version-not-supported+ 505)

(defun send-unknown-protocol-response (stream &optional additional-info)
  "Send a ``HTTP Version Not Supported'' response to the client."
  (write-sequence (flex:string-to-octets
		   (format nil "HTTP/1.0 ~D ~A~C~CConnection: close~C~C~C~CYour request could not be interpreted by this HTTP server~C~C~@[~A~]~C~C"
			   +http-version-not-supported+ (http-status-message +http-version-not-supported+) #\Return #\Linefeed
			   #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed additional-info #\Return #\Linefeed))
		  stream))

(defun printable-ascii-char-p (char)
  (<= 32 (char-code char) 126))

(defun read-http-headers (stream &optional log-stream)
  "Reads HTTP header lines from STREAM \(except for the initial
status line which is supposed to be read already) and returns a
corresponding alist of names and values where the names are
keywords and the values are strings.  Multiple lines with the
same name are combined into one value, the individual values
separated by commas.  Header lines which are spread across
multiple lines are recognized and treated correctly.  Additonally
logs the header lines to LOG-STREAM if it is not NIL."
  (let (headers)
    (labels ((read-header-line ()
               "Reads one header line, considering continuations."
               (with-output-to-string (header-line)
                 (loop
                   (let ((line (trim-whitespace (io/chunky::read-line* stream log-stream))))
                     (when (zerop (length line))
                       (return))
                     (write-sequence line header-line)
                     (let ((next (peek-char* stream)))
                       (unless (io/chunky::whitespacep next)
                         (return)))
                     ;; we've seen whitespace starting a continutation,
                     ;; so we loop
                     (write-char #\Space header-line)))))
             (split-header (line)
               "Splits line at colon and converts it into a cons.
Returns NIL if LINE consists solely of whitespace."
               (unless (zerop (length (trim-whitespace line)))
                 (let ((colon-pos (or (position #\: line :test #'char=)
                                      (error 'syntax-error
                                             :stream stream
                                             :format-control "Couldn't find colon in header line ~S."
                                             :format-arguments (list line)))))
                   (cons (codec::http-keyword* (subseq line 0 colon-pos))
                         (trim-whitespace (subseq line (1+ colon-pos)))))))
             (add-header (pair)
               "Adds the name/value cons PAIR to HEADERS.  Takes
care of multiple headers with the same name."
               (let* ((name (car pair))
                      (existing-header (assoc name headers :test #'eq))
                      (existing-value (cdr existing-header)))
                 (cond (existing-header
                        (setf (cdr existing-header)
                              (format nil "~A~:[,~;~]~A"
                                      existing-value
                                      (and *treat-semicolon-as-continuation*
                                           (eq name :set-cookie)
                                           (std/seq::ends-with-p (trim-whitespace existing-value) ";"))
                                      (cdr pair))))
                       (t (push pair headers))))))
      (loop for header-pair = (split-header (read-header-line))
            while header-pair
            do (add-header header-pair)))
    (nreverse headers)))

(defun get-http-request-data (stream)
  "Reads incoming headers from the client via STREAM.  Returns as
multiple values the headers as an alist, the method, the URI, and the
protocol of the request."
  (with-character-stream-semantics
    (let ((first-line (read-line stream)))
      (when first-line
        (unless (every #'printable-ascii-char-p first-line)
          (send-bad-request-response stream "Non-ASCII character in request line")
          (return-from get-http-request-data nil))
        (destructuring-bind (&optional method url-string protocol)
            (cl-ppcre:split "\\s+" first-line :limit 3)
          (cond ((not
                  (setf method
                        (find method +known-http-methods+ :test #'string-equal)))
                 (send-bad-request-response stream)
                 (return-from get-http-request-data nil))
                ((not url-string)
                 (send-bad-request-response stream)
                 (return-from get-http-request-data nil))
                ((not protocol)
                 ;; HTTP/1.1 specifies that if protocol is not provided
                 ;; then assume protocol version to be 1.0
                 (setf protocol :http/1.0))
                ((not
                  (setf protocol
                        (find protocol +known-http-versions+ :test #'string-equal)))
                 (send-unknown-protocol-response stream)
                 (return-from get-http-request-data nil)))
          (when *header-stream*
            (format *header-stream* "~A~%" first-line))
          (let ((headers (read-http-headers stream *header-stream*)))
            ;; maybe handle 'Expect: 100-continue' header
            (when-let ((expectations (cdr #|assoc*|# (assoc :expect headers))))
              (when (member "100-continue" (ppcre:split "\\s*,\\s*" expectations) :test #'equalp)
                ;; according to 14.20 in the RFC - we should actually
                ;; check if we have to respond with 417 here
                (let ((continue-line
                        (format nil "HTTP/1.1 ~D ~A"
                                codec::+http-continue+
                                (http-status-message codec::+http-continue+))))
                  (write-sequence (map 'list #'char-code continue-line) stream)
                  (write-sequence std/string::+crlf+ stream)
                  (write-sequence std/string::+crlf+ stream)
                  (force-output stream)
                  (when *header-stream*
                    (format *header-stream* "~A~%" continue-line)))))
            (values headers method url-string protocol)))))))

(defmethod process-connection ((*service* http-service) (socket t))
  (let* ((socket-stream (sb-bsd-sockets:socket-make-stream socket))
         (*service-stream*)
         (*close-service-stream* t)
         (remote (multiple-value-list (sb-bsd-sockets:socket-peername socket)))
         (local (multiple-value-list (sb-bsd-sockets:socket-name socket))))
    (unwind-protect
         ;; process requests until shutdown signal is received or the peer
         ;; fails to send a request
         (progn
           (setq *service-stream* (initialize-connection-hook *service* socket-stream))
           (loop
             (let ((*finish-processing-socket* t))
               (when (shutdown-p *service*)
                 (return))
               (multiple-value-bind (headers-in method url-string protocol)
                   (get-http-request-data *service-stream*)
                 ;; check if there was a request at all
                 (unless method
                   (return))
                 (let ((*response* (make-instance (service-response-class *service*)))
                       (*session* nil)
                       (transfer-encodings (cdr #|assoc*?|# 
                                            (assoc :transfer-encoding headers-in))))
                   (when transfer-encodings
                     (setq transfer-encodings
                           (cl-ppcre:split "\\s*,\\s*" transfer-encodings))
                     (when (member "chunked" transfer-encodings :test #'equalp)
                       (setf *service-stream* (io/chunky:make-chunked-stream *service-stream*))))
                   (with-request-count-incf *service*
                     (process-request 
                      (service-make-request *service* socket
                                            :headers-in headers-in
                                            :content-stream *service-stream*
                                            :method method
                                            :uri url-string
                                            :remote remote
                                            :local local
                                            :protocol protocol))))
                 (finish-output *service-stream*)
                 (setq *service-stream* (reset-connection-stream *service* *service-stream*))
                 (when *finish-processing-socket*
                   (return))))))
      (when *close-service-stream*
        (flet ((close-stream (stream)
                 ;; as we are at the end of the request here, we ignore all
                 ;; errors that may occur while flushing and/or closing the
                 ;; stream.
                 (ignore-errors
                  (finish-output stream))
                 (ignore-errors
                  (close stream :abort t))))
          (unless (or (not *service-stream*)
                      (eql socket-stream *service-stream*))
            (close-stream *service-stream*))
          (close-stream socket-stream))))))

;; FIX 2024-12-28: do better :)
(defun wake-tcp-service-for-shutdown (service)
  "Create a dummy connection to the service, waking ACCEPT while it
is waiting. The idea is to force a check of SHUTDOWN-P."
  (handler-case
      (multiple-value-bind (address port) (sb-bsd-sockets:socket-name (net/srv::socket service))
        (let ((conn (sb-bsd-sockets:socket-connect
                     (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)
                     (cond
                       ((and (= (length address) 4) (zerop (elt address 0)))
                        #(127 0 0 1))
                       ((and (= (length address) 16)
                             (every #'zerop address))
                        #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
                       (t address))
                     port)))
          (sb-bsd-sockets:socket-close conn)))
    (error (e)
      (service-log-message service :error "Wake-for-shutdown connect failed: ~A" e))))

(defmethod stop ((self http-service) &key)
  (wake-tcp-service-for-shutdown self))

(defclass ssl-service (net-service)
  ((cert-file :initarg :cert-file
              :reader cert-file
              :initform nil)
   (key-file :initarg :key-file
             :reader key-file
             :initform nil)
   (password :initarg :password
             :reader password))
  (:default-initargs
   :password nil
   :port 443
   :key-file *default-ssl-key-file*))

(defmethod initialize-instance :after ((self ssl-service) &rest initargs)
  (declare (ignore initargs))
  (when-let ((key-file (slot-value self 'key-file)))
    (setf (slot-value self 'key-file)
          (namestring (truename key-file))))
  (when-let ((cert-file (slot-value self 'cert-file)))
    (setf (slot-value self 'cert-file)
          (namestring (truename cert-file)))))

(defmethod secure-service-p ((self ssl-service))
  (declare (ignore self))
  t)

(defmethod initialize-connection-hook ((self ssl-service) stream)
  (call-next-method self
                    (apply 'ssl:make-ssl-server-stream
                           stream
                           `(,@(when-let ((cf (cert-file self)))
                                 `(:certificate ,cf))
                             ,@(when-let ((kf (key-file self)))
                                 `(:key ,kf))
                             ,@(when-let ((pw (password self)))
                                 `(:password ,pw))))))

(defun get-peer-ssl-certificate ()
  (ssl:ssl-stream-x509-certificate *service-stream*))

(defclass https-service (http-service ssl-service) ()
  (:default-initargs
   :engine (make-instance 'thread-per-connection-engine :name :https)))
