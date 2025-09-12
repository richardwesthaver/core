;;; http.lisp --- HTTP Codec Primitives

;; Basic HTTP Codec Support

;;; Code:
(in-package :net/codec/http)

(eval-always
  (defvar *http-status-message-map* (make-hash-table)
    "Used to map numerical return codes to message strings.")
  (defun http-status-message (i)
    (gethash i *http-status-message-map*)))

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

;; from CHUNGA
(eval-always
  (defun make-keyword (string destructivep)
    "Converts the string STRING to a keyword where all characters are
uppercase or lowercase, taking into account the current readtable
case.  Destructively modifies STRING if DESTRUCTIVEP is true."
    (intern (funcall
             (if destructivep
                 (if (eq (readtable-case *readtable*) :upcase)
                     #'nstring-upcase
                     #'nstring-downcase)
                 (if (eq (readtable-case *readtable*) :upcase)
                     #'string-upcase
                     #'string-downcase))
             string)
            :keyword))
  (define-constant +known-http-words+
      (list ;; headers including WebDAV and some de facto standard headers
       "Accept"
       "Accept-Charset"
       "Accept-Encoding"
       "Accept-Language"
       "Accept-Ranges"
       "Age"
       "Allow"
       "Authorization"
       "Cache-Control"
       "Connection"
       "Content-Encoding"
       "Content-Language"
       "Content-Length"
       "Content-Location"
       "Content-MD5"
       "Content-Range"
       "Content-Type"
       "DAV"
       "Date"
       "Depth"
       "Destination"
       "ETag"
       "Expect"
       "Expires"
       "From"
       "Host"
       "If"
       "If-Match"
       "If-Modified-Since"
       "If-None-Match"
       "If-Range"
       "If-Unmodified-Since"
       "Last-Modified"
       "Location"
       "Lock-Token"
       "Max-Forwards"
       "Overwrite"
       "Pragma"
       "Proxy-Authenticate"
       "Proxy-Authorization"
       "Range"
       "Referer"
       "Retry-After"
       "Server"
       "TE"
       "TimeOut"
       "Trailer"
       "Transfer-Encoding"
       "Upgrade"
       "User-Agent"
       "Vary"
       "Via"
       "WWW-Authenticate"
       "Warning"
       ;; methods including WebDAV
       "CONNECT"
       "COPY"
       "DELETE"
       "GET"
       "HEAD"
       "LOCK"
       "MKCOL"
       "MOVE"
       "OPTIONS"
       "POST"
       "PROPFIND"
       "PROPPATCH"
       "PUT"
       "TRACE"
       "UNLOCK"
       ;; protocols
       "HTTP/1.1"
       "HTTP/1.0"
       ;; only a few and only the "preferred MIME names" - see
       ;; <http://www.iana.org/assignments/character-sets> for a
       ;; complete list
       "US-ASCII"
       "ISO-8859-1"
       "UTF-8"
       "UTF-16"
       "UTF-32BE"
       "UTF-32LE")
    :test (lambda (a b) (every 'string= a b))
    :documentation
    "A list of words \(headers, methods, protocols, character sets)
that are typically seen in HTTP communication.  Mostly from RFC 2616,
but includes WebDAV stuff and other things as well."))

(define-constant +http-keyword-table+
    (let ((hash (make-hash-table :test 'equal :size (length +known-http-words+))))
      (loop for word in +known-http-words+
            do (setf (gethash word hash) (make-keyword word nil)))
      hash)
  :test (lambda (a b) (equalp (hash-table-alist a) (hash-table-alist b)))
  :documentation
  "A hash table which case-insensitively maps the strings from
+KNOWN-HTTP-WORDS+ to keywords.")

(defun http-keyword (string &key (destructivep t))
  "Converts the string STRING to a keyword where all characters are
uppercase or lowercase, taking into account the current readtable
case.  Might destructively modify STRING if DESTRUCTIVEP is true which
is the default.  \"Knows\" several HTTP header names and methods and
is optimized to not call INTERN for these."
  (or (gethash string +http-keyword-table+)
      (make-keyword string destructivep)))

(std:definline http-keyword* (string)
  (or (find-symbol (string-upcase string) (find-package "KEYWORD"))
      string))

(define-constant +known-http-methods+
    #(:get :post :head :put :delete :connect :options :trace :patch)
  :test 'equalp)

(define-constant +known-http-versions+ #(:http/1.0 :http/1.1) :test 'equalp)

