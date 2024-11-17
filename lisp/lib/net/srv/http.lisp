;;; http.lisp --- HTTP Services

;; HTTP/S Service (based on Hunchentoot)

;;; Commentary:

;; This module contains the main HTTP/S web application server machinery for
;; core modules. Loading this file should give you the basics needed to build
;; a CLOS-based asynchronous web server.

;;; Code:
(in-package :net/srv/http)

(defvar *default-content-type* "text/html")
(eval-always
  (defvar *http-status-message-map* (make-hash-table)
    "Used to map numerical return codes to message strings.")
  (defun http-status-message (i)
    (gethash i *http-status-message-map*)))

;;; Utils
(defun ssl-p (&optional (service *service*))
  (and (secure-service-p service)
       (eql :https (socket-protocol (socket service)))))

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
(defclass http-service-response (response) ((response :type http-response)))

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

(defun session-expired-p (session)
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

STATUS-CODE is the HTTP status code used in the response, HEADERS and COOKIES are used to generate the header. If CONTENT is provided, it is used as the body.

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

#+ssl
(defclass ssl-service (service)
  ((certificate-file :initarg :certificate-file
                     :reader certificate-file)
   (privatekey-file :initarg :privatekey-file
                    :reader privatekey-file)
   (privatekey-password :initarg :privatekey-password
                        :reader privatekey-password))
  (:default-initargs
   :password nil
   :port 443))

(defmethod initialize-instance :after ((self ssl-service) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value self 'privatekey-file)
        (namestring (truename (privatekey-file self)))
        (slot-value self 'certificate-file)
        (namestring (truename (certificate-file self)))))

(defmethod secure-service-p ((self ssl-service))
  (declare (ignore self))
  t)

(defmethod initialize-connection-hook ((self ssl-service) stream)
  (call-next-method self
                    (cl+ssl:make-ssl-server-stream
                     stream
                     :certificate (certificate-file self)
                     :key (privatekey-file self)
                     :password (privatekey-password self))))

(defun get-peer-ssl-certificate ()
  (cl+ssl:ssl-stream-x509-certificate *service-stream*))
