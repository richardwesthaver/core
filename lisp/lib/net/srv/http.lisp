;;; http.lisp --- HTTP Services

;; HTTP/S Service (based on Hunchentoot)

;;; Commentary:

;; This module contains the main HTTP/S web application server machinery for
;; core modules. Loading this file should give you the basics needed to build
;; a CLOS-based asynchronous web server.

;;; Code:
(in-package :net/srv/http)

(defvar *default-content-type* "text/html")
(defvar *header-stream* nil)

(eval-always
  (defvar *http-status-message-map* (make-hash-table)
    "Used to map numerical return codes to message strings.")
  (defun http-status-message (i)
    (gethash i *http-status-message-map*)))

;;; Utils
(defun ssl-p (&optional (service *service*))
  (and (secure-service-p service)
       (eql :https (sb-bsd-sockets:socket-protocol (sb-bsd-sockets:socket service)))))

;;; Return Codes
(defmacro def-http-return-code (name value message)
  "Shortcut to define constants for return codes.  NAME is a
Lisp symbol, VALUE is the numerical value of the return code, and
MESSAGE is the phrase \(a string) to be shown in the
server's status line."
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (defconstant ,name ,value ,(format nil "HTTP return code \(~A) for '~A'."
                                        value message))
     (setf (gethash ,value *http-status-message-map*) ,message)))

(def-http-return-code +http-continue+ 100 "Continue")
(def-http-return-code +http-switching-protocols+ 101 "Switching Protocols")
(def-http-return-code +http-processing+ 102 "Processing")
(def-http-return-code +http-ok+ 200 "OK")
(def-http-return-code +http-created+ 201 "Created")
(def-http-return-code +http-accepted+ 202 "Accepted")
(def-http-return-code +http-non-authoritative-information+ 203 "Non-Authoritative Information")
(def-http-return-code +http-no-content+ 204 "No Content")
(def-http-return-code +http-reset-content+ 205 "Reset Content")
(def-http-return-code +http-partial-content+ 206 "Partial Content")
(def-http-return-code +http-multi-status+ 207 "Multi-Status")
(def-http-return-code +http-already-reported+ 208 "Already Reported")
(def-http-return-code +http-im-used+ 226 "IM Used")
(def-http-return-code +http-multiple-choices+ 300 "Multiple Choices")
(def-http-return-code +http-moved-permanently+ 301 "Moved Permanently")
(def-http-return-code +http-moved-temporarily+ 302 "Moved Temporarily")
(def-http-return-code +http-see-other+ 303 "See Other")
(def-http-return-code +http-not-modified+ 304 "Not Modified")
(def-http-return-code +http-use-proxy+ 305 "Use Proxy")
(def-http-return-code +http-temporary-redirect+ 307 "Temporary Redirect")
(def-http-return-code +http-permanent-redirect+ 308 "Permanent Redirect")
(def-http-return-code +http-bad-request+ 400 "Bad Request")
(def-http-return-code +http-authorization-required+ 401 "Authorization Required")
(def-http-return-code +http-payment-required+ 402  "Payment Required")
(def-http-return-code +http-forbidden+ 403 "Forbidden")
(def-http-return-code +http-not-found+ 404 "Not Found")
(def-http-return-code +http-method-not-allowed+ 405 "Method Not Allowed")
(def-http-return-code +http-not-acceptable+ 406 "Not Acceptable")
(def-http-return-code +http-proxy-authentication-required+ 407 "Proxy Authentication Required")
(def-http-return-code +http-request-time-out+ 408 "Request Time-out")
(def-http-return-code +http-conflict+ 409 "Conflict")
(def-http-return-code +http-gone+ 410 "Gone")
(def-http-return-code +http-length-required+ 411 "Length Required")
(def-http-return-code +http-precondition-failed+ 412 "Precondition Failed")
(def-http-return-code +http-request-entity-too-large+ 413 "Request Entity Too Large")
(def-http-return-code +http-request-uri-too-large+ 414 "Request-URI Too Large")
(def-http-return-code +http-unsupported-media-type+ 415 "Unsupported Media Type")
(def-http-return-code +http-requested-range-not-satisfiable+ 416 "Requested range not satisfiable")
(def-http-return-code +http-expectation-failed+ 417 "Expectation Failed")
(def-http-return-code +http-im-a-teapot+ 418 "I'm a teapot")
(def-http-return-code +http-misdirected-request+ 421 "Misdirected Request")
(def-http-return-code +http-unprocessable-entity+ 422 "Unprocessable Entity")
(def-http-return-code +http-locked+ 423 "Locked")
(def-http-return-code +http-failed-dependency+ 424 "Failed Dependency")
(def-http-return-code +http-upgrade-required+ 426 "Upgrade Required")
(def-http-return-code +http-precondition-required+ 428 "Precondition Required")
(def-http-return-code +http-too-many-requests+ 429 "Too Many Requests")
(def-http-return-code +http-request-header-fields-too-large+ 431 "Request Header Fields Too Large")
(def-http-return-code +http-connection-closed-without-response+ 444 "Connection Closed Without Response")
(def-http-return-code +http-unavailable-for-legal-reasons+ 451 "Unavailable For Legal Reasons")
(def-http-return-code +http-client-closed-request+ 499 "Client Closed Request")
(def-http-return-code +http-internal-server-error+ 500 "Internal Server Error")
(def-http-return-code +http-not-implemented+ 501 "Not Implemented")
(def-http-return-code +http-bad-gateway+ 502 "Bad Gateway")
(def-http-return-code +http-service-unavailable+ 503 "Service Unavailable")
(def-http-return-code +http-gateway-time-out+ 504 "Gateway Time-out")
(def-http-return-code +http-version-not-supported+ 505 "Version not supported")
(def-http-return-code +http-variant-also-negotiates+ 506 "Variant Also Negotiates")
(def-http-return-code +http-insufficient-storage+ 507 "Insufficient Storage")
(def-http-return-code +http-loop-detected+ 508 "Loop Detected")
(def-http-return-code +http-not-extended+ 510 "Not Extended")
(def-http-return-code +http-network-authentication-required+ 511 "Network Authentication Required")
(def-http-return-code +http-network-connect-timeout-error+ 599 "Network Connect Timeout Error")

;;; Response
(defclass http-service-response (response) 
  ((response :type http-response)
   (content-type :reader content-type
                 :documentation "The outgoing 'Content-Type' http
header which defaults to the value of *DEFAULT-CONTENT-TYPE*.")
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

(defmethod response-ok-p ((res http-service-response))
  (eql (response-status (slot-value *response* 'response)) +http-ok+))

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

;; content-type
;; content-length *
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

(defclass http-service-request (request)
  ((request :type http-request)))

(defun start-http-output (status &optional (content nil contentp))
  "Sends all headers and maybe the content body to *SERBICE-STREAM*. Returns
immediately and does nothing if called more than once per request. Called by
PROCESS-REQUEST and/or SEND-HEADERS. The STATUS argument represents the
integer return code of the request. The corresponding reason phrase is
determined by calling the HTTP-STATUS-MESSAGE function. The CONTENT provided
represents the body data to send to the client, if any. If it is not
specified, no body is written to the client. The handler function is expected
to directly write to the stream in this case.

Returns the stream that is connected to the client."
  (let* ((chunkedp (and (output-chunking-p *service*)
                        (eq (server-protocol *request*) :http/1.1)
                        ;; only turn chunking on if the content
                        ;; length is unknown at this point...
                        (null (or (content-length*) contentp))))
         (request-method (request-method *request*))
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
                  (eql (response-status*) +http-not-modified+)
                  (content-length*)
                  content)))
      ;; now set headers for keep-alive and chunking
      (when chunkedp
        (setf (header-out :transfer-encoding) "chunked"))
      (cond (keep-alive-p
             (setf *finish-processing-socket* nil)
             (when (and (service-read-timeout *service*)
                        (or (not (eq (server-protocol *request*) :http/1.1))
                            keep-alive-requested-p))
               ;; persistent connections are implicitly assumed for
               ;; HTTP/1.1, but we return a 'Keep-Alive' header if the
               ;; client has explicitly asked for one
               (unless (header-out :connection) ; allowing for handler overriding
                 (setf (header-out :connection) "Keep-Alive"))
               (setf (header-out :keep-alive)
                     (format nil "timeout=~D" (service-read-timeout *service*)))))
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
      ;; if the Content-Type header starts with one of the strings
      ;; in *CONTENT-TYPES-FOR-URL-REWRITE* then maybe rewrite the
      ;; content
      (setq content (maybe-rewrite-urls-for-session content)))
    (when (stringp content)
      ;; if the content is a string, convert it to the proper external format
      (setf content (sb-ext:string-to-octets content :external-format (response-external-format*))
            (content-type*) (maybe-add-charset-to-content-type-header (content-type*)
                                                                      (response-external-format*))))
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
               (net/srv:log-message* log:*log-level* "~A~@[~%~A~]" error (when log:*log-show-backtrace*
                                                               backtrace)))
                    (start-http-output +http-internal-server-error+
                                       (service-status-message 
                                        *service*
                                        +http-internal-server-error+
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
                         (session-start session)))

(defmethod session-expired-p ((self http-session))
  (< (+ (last-click session) (session-timeout session))
     (get-universal-time)))

(defun session-gc ()
  "Removes sessions from the global session database which have expired or are invalid."
  (with-session-db-lock (session-db-lock *service*)
    (setf (session-db *service*)
          (loop for pair in (session-db *service*)
                for (nil . session) = pair
                when (session-expired-p session)
                do (remove-session-hook *service* session)
                else
                collect pair)))
  (values))

(defun get-session (id)
  (let ((session
          (cdr (assoc id (session-db *service*) :test #'=))))
    (when (and session
               (session-expired-p session))
      (when *response*
        (log-message* :info "Session with ID ~A too old" id))
      (remove-session session)
      (setq session nil))
    session))

(defgeneric session-cookie-value (session)
  (:method ((session session))
    (and session
         (format nil
                 "~D:~A"
                 (id:id session)
                 (stringify-session session)))))

(defun refresh-session-cookie-value (session)
  (setf (slot-value session 'session-start) (get-universal-time)
        (slot-value session 'session-string) (stringify-session session))
  (set-cookie (session-cookie-name *service*)
              :value (session-cookie-value session)
              :path "/"
              :http-only t))

(defun html-session-hook ()
  (set-cookie (session-cookie-name *session*)
              :value (session-cookie-value *session*)
              :path "/"
              :http-only t))

;;; Service
(defclass http-service (service) 
   ;; RESEARCH 2024-07-18: 
   ;; may need to start dealing with this
   ;; https://datatracker.ietf.org/doc/html/rfc2616#section-3.6.1
  ((chunk-output-p :type boolean :initarg :chunk-output-p)
   (chunk-input-p :type boolean :initarg :chunk-input-p))
  (:default-initargs
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
            (authorization)
            (iso-time)
            (request-method*)
            (script-name*)
            (query-string*)
            (server-protocol*)
            code
            (content-length*)
            (referer)
            (user-agent))))

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
          do (write-header-line (string-upcase k) v hstream))
    ;; cookies
    (loop for (nil . cookie) in cookies
          do (write-header-line "Set-Cookie" (stringify-cookie cookie) hstream))
    (format hstream "~C~C" #\Return #\Linefeed))
  (when content
    (write-sequence content stream)
    (finish-output stream))
  stream)

(defun get-http-request-data (stream)
  "Reads incoming headers from the client via STREAM.  Returns as
multiple values the headers as an alist, the method, the URI, and the
protocol of the request."
  (with-character-stream-semantics
    (let ((first-line (read-initial-request-line stream)))
      (when first-line
        (unless (every #'printable-ascii-char-p first-line)
          (send-bad-request-response stream "Non-ASCII character in request line")
          (return-from get-http-request-data nil))
        (destructuring-bind (&optional method url-string protocol)
            (split "\\s+" first-line :limit 3)
          (cond ((not
                  (setf method
                        (find method +valid-request-methods+ :test #'string-equal)))
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
                        (find protocol +valid-protocol-versions+ :test #'string-equal)))
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
                                +http-continue+
                                (http-status-message +http-continue+))))
                  (write-sequence (map 'list #'char-code continue-line) stream)
                  (write-sequence std/string::+crlf+ stream)
                  (write-sequence std/string::+crlf+ stream)
                  (force-output stream)
                  (when *header-stream*
                    (format *header-stream* "~A~%" continue-line)))))
            (values headers method url-string protocol)))))))

#+ssl
(defclass ssl-service (service)
  ((cert-file :initarg :cert-file
                     :reader cert-file)
   (key-file :initarg :key-file
                    :reader key-file)
   (password :initarg :password
                        :reader password))
  (:default-initargs
   :password nil
   :port 443))

(defmethod initialize-instance :after ((self ssl-service) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value self 'key-file)
        (namestring (truename (key-file self)))
        (slot-value self 'cert-file)
        (namestring (truename (cert-file self)))))

(defmethod secure-service-p ((self ssl-service))
  (declare (ignore self))
  t)

(defmethod initialize-connection-hook ((self ssl-service) stream)
  (call-next-method self
                    (cl+ssl:make-ssl-server-stream
                     stream
                     :certificate (cert-file self)
                     :key (key-file self)
                     :password (password self))))

(defun get-peer-ssl-certificate ()
  (cl+ssl:ssl-stream-x509-certificate *service-stream*))

(defclass https-service (http-service ssl-service) ())
