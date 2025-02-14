;;; http.lisp --- HTTP Codec Primitives

;; Basic HTTP Codec Support

;;; Code:
(in-package :net/codec/http)

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
