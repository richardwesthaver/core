;;; net/req.lisp --- HTTP Request API

;; based on Fukamachi's DEXADOR

;;; Code:
(in-package :net/req)

;;; errors
(define-condition http-request-failed (error)
  ((body :initarg :body
         :reader response-body)
   (status :initarg :status
           :reader response-status)
   (headers :initarg :headers
            :reader response-headers)
   (uri :initarg :uri
        :reader request-uri)
   (method :initarg :method
           :reader request-method))
  (:report (lambda (condition stream)
             (with-slots (uri status) condition
               (format stream "An HTTP request to ~S has failed (status=~D)."
                       (render-uri uri nil)
                       status)))))

(defmacro define-request-failed-condition (name code)
  `(define-condition ,(intern (format nil "~A-~A" :http-request name)) (http-request-failed)
     ()
     (:report (lambda (condition stream)
                (with-slots (body uri) condition
                  (format stream ,(format nil "An HTTP request to ~~S returned ~D ~A.~~2%~~A"
                                          code
                                          (substitute #\Space #\- (string-downcase name)))
                          (render-uri uri nil)
                          body))))))


(defvar *request-failed-error* (make-hash-table :test 'eql))

#.`(progn
     ,@(loop for (name . code) in '(;; 4xx (Client Errors)
                                    (bad-request                   . 400)
                                    (unauthorized                  . 401)
                                    (payment-required              . 402)
                                    (forbidden                     . 403)
                                    (not-found                     . 404)
                                    (method-not-allowed            . 405)
                                    (not-acceptable                . 406)
                                    (proxy-authentication-required . 407)
                                    (request-timeout               . 408)
                                    (conflict                      . 409)
                                    (gone                          . 410)
                                    (length-required               . 411)
                                    (precondition-failed           . 412)
                                    (payload-too-large             . 413)
                                    (uri-too-long                  . 414)
                                    (unsupported-media-type        . 415)
                                    (range-not-satisfiable         . 416)
                                    (expectation-failed            . 417)
                                    (misdirected-request           . 421)
                                    (upgrade-required              . 426)
                                    (too-many-requests             . 429)

                                    ;; 5xx (Server Errors)
                                    (internal-server-error      . 500)
                                    (not-implemented            . 501)
                                    (bad-gateway                . 502)
                                    (service-unavailable        . 503)
                                    (gateway-timeout            . 504)
                                    (http-version-not-supported . 505))
             collect `(define-request-failed-condition ,name ,code)
             collect `(setf (gethash ,code *request-failed-error*)
                            ',(intern (format nil "~A-~A" :http-request name)))))

(defun http-request-failed (status &key body headers uri method)
  (cerror
   "Ignore and continue"
   (gethash status *request-failed-error* 'http-request-failed)
   :body body
   :status status
   :headers headers
   :uri uri
   :method method))

(define-condition socks5-proxy-request-failed (http-request-failed)
  ((reason :initarg :reason))
  (:report (lambda (condition stream)
             (with-slots (uri reason) condition
               (format stream "An HTTP request to ~S via SOCKS5 has failed (reason=~S)."
                       (render-uri uri nil)
                       reason)))))

;;; utils
(defvar *default-connect-timeout* 10)
(defvar *default-read-timeout* 10)
(defvar *verbose* nil)
(defvar *no-ssl* nil)

(defvar *default-proxy* (or #-windows (uiop:getenv "HTTPS_PROXY")
                            #-windows (uiop:getenv "HTTP_PROXY"))
  "If specified will be used as the default value of PROXY in calls to dexador.  Defaults to
 the value of the environment variable HTTPS_PROXY or HTTP_PROXY if not on Windows.")

(define-constant +crlf+ (string-to-octets (format nil "~C~C" #\Return #\Newline)) :test 'equalp)

(eval-always
  (defparameter *default-user-agent*
    (format nil "CC/req (~A~@[ ~A~]); ~A;~@[ ~A~]"
            (lisp-implementation-type)
            (lisp-implementation-version)
            (software-type)
            (software-version))))

(defparameter *header-buffer* nil)

(defun write-first-line (method uri version &optional (buffer *header-buffer*))
  (fast-write-sequence (string-to-octets (string method)) buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (string-to-octets
                         (format nil "~A~:[~;~:*?~A~]"
                                 (or (uri-path uri) "/")
                                 (uri-query uri)))
                       buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (ecase version
                         (1.1 (string-to-octets "HTTP/1.1"))
                         (1.0 (string-to-octets "HTTP/1.0")))
                       buffer)
  (fast-write-sequence +crlf+ buffer))

(defun write-header-field (name buffer)
  (fast-write-sequence (if (typep name 'octet-vector)
                           name
                           (string-to-octets (string-capitalize name)))
                       buffer))

(defun write-header-value (value buffer)
  (fast-write-sequence (if (typep value 'octet-vector)
                           value
                           (string-to-octets (princ-to-string value)))
                       buffer))

(defun write-header (name value &optional (buffer *header-buffer*))
  (write-header-field name buffer)
  (fast-write-sequence (string-to-octets ": ") buffer)
  (write-header-value value buffer)
  (fast-write-sequence +crlf+ buffer))

(define-compiler-macro write-header (name value &optional (buffer '*header-buffer*))
  `(progn
     ,(if (and (constantp name)
               (typep name '(or keyword string)))
          `(fast-write-sequence (string-to-octets ,(string-capitalize name)) ,buffer)
          `(write-header-field ,name ,buffer))
     (fast-write-sequence (string-to-octets ": ") ,buffer)
     ,(if (constantp value)
          `(fast-write-sequence (string-to-octets ,(string value)) ,buffer)
          `(write-header-value ,value ,buffer))
     (fast-write-sequence +crlf+ ,buffer)))

(defmacro with-header-output ((buffer &optional output) &body body)
  `(with-fast-output (,buffer ,output)
     (declare (ignorable ,buffer))
     (let ((*header-buffer* ,buffer))
       ,@body)))

(defun write-connect-header (uri version buffer &optional proxy-auth)
  (fast-write-sequence (string-to-octets "CONNECT") buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (string-to-octets (format nil "~A:~A"
                                                       (uri-host uri)
                                                       (uri-port uri)))
                       buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (ecase version
                         (1.1 (string-to-octets "HTTP/1.1"))
                         (1.0 (string-to-octets "HTTP/1.0")))
                       buffer)
  (fast-write-sequence +crlf+ buffer)
  (fast-write-sequence (string-to-octets "Host:") buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (string-to-octets (format nil "~A:~A"
                                                       (uri-host uri)
                                                       (uri-port uri)))
                       buffer)
  (when proxy-auth
    (fast-write-sequence +crlf+ buffer)
    (fast-write-sequence (string-to-octets "Proxy-Authorization:") buffer)
    (fast-write-byte #.(char-code #\Space) buffer)
    (fast-write-sequence (string-to-octets proxy-auth) buffer))
  (fast-write-sequence +crlf+ buffer)
  (fast-write-sequence +crlf+ buffer))

(defun make-random-string (&optional (length 12))
  (declare (type fixnum length))
  (let ((result (make-string length)))
    (declare (type simple-string result))
    (dotimes (i length result)
      (setf (aref result i)
            (ecase (random 5)
              ((0 1) (code-char (+ #.(char-code #\a) (random 26))))
              ((2 3) (code-char (+ #.(char-code #\A) (random 26))))
              ((4) (code-char (+ #.(char-code #\0) (random 10)))))))))

;;; encoding
(defun parse-content-type (content-type)
  (let ((types
          (nth-value 1
                     (ppcre:scan-to-strings "^\\s*?(\\w+)/([^;\\s]+)(?:\\s*;\\s*charset=([A-Za-z0-9_-]+))?"
                                            content-type))))
    (when types
      (values (aref types 0)
              (aref types 1)
              (aref types 2)))))

(defun charset-to-encoding (charset &optional
                                    (default sb-ext:*default-external-format*))
  (cond
    ((null charset)
     default)
    ((string-equal charset "utf-8")
     :utf-8)
    ((string-equal charset "euc-jp")
     :eucjp)
    ((or (string-equal charset "shift_jis")
         (string-equal charset "shift-jis"))
     :cp932)
    ((string-equal charset "windows-31j")
     :cp932)
    (t (or (when (sb-impl::get-external-format (keywordicate charset)) charset)
           default))))

(defun detect-charset (content-type body)
  (multiple-value-bind (type subtype charset)
      (parse-content-type content-type)
    (cond
      ((charset-to-encoding charset nil))
      ((string-equal type "text")
       (or (charset-to-encoding charset nil)
           (if (and (string-equal subtype "html")
                    (typep body '(array (unsigned-byte 8) (*))))
               (charset-to-encoding (detect-charset-from-html body) nil)
               nil)
           :utf-8))
      ((and (string-equal type "application")
            (or (string-equal subtype "json")
                (string-equal subtype "javascript")))
       ;; According to RFC4627 (http://www.ietf.org/rfc/rfc4627.txt),
       ;; JSON text SHALL be encoded in Unicode. The default encoding is UTF-8.
       ;; It's possible to determine if the encoding is UTF-16 or UTF-36
       ;; by looking at the first four octets, however, I leave it to the future.
       ;;
       ;; According to RFC4329 (https://datatracker.ietf.org/doc/html/rfc4329),
       ;; javascript also is specified by charset, or defaults to UTF-8
       ;; It's also possible to specify in the first four octets, but
       ;; like application/json I leave it to the future.
       (charset-to-encoding charset :utf-8))
      ((and (string-equal type "application")
            (ppcre:scan "(?:[^+]+\\+)?xml" subtype))
       (charset-to-encoding charset)))))

(defun detect-charset-from-html (body)
  "Detect the body's charset by (roughly) searching meta tags which has \"charset\" attribute."
  (labels ((find-meta (start)
             (search #.(string-to-octets "<meta ") body :start2 start))
           (main (start)
             (let ((start (find-meta start)))
               (unless start
                 (return-from main nil))
               (let ((end (position (char-code #\>) body :start start :test #'=)))
                 (unless end
                   (return-from main nil))
                 (incf end)
                 (let ((match (nth-value 1 (ppcre:scan-to-strings
                                            "charset=[\"']?([^\\s\"'>]+)[\"']?"
                                            (octets-to-string body :start start :end end)))))
                   (if match
                       (aref match 0)
                       (main end)))))))
    (main 0)))

;;; keep-alive-stream
(defclass keep-alive-stream (fundamental-input-stream)
  ((stream :type (or null stream)
           :initarg :stream
           :initform (error ":stream is required")
           :accessor keep-alive-stream-stream
           :documentation "A stream; when we read END elements from it, we call CLOSE-ACTION on it and
   set this slot to nil.")
   (end :initarg :end
        :initform nil
        :accessor keep-alive-stream-end)
   (close-action :initarg :on-close-or-eof :reader close-action
                 :documentation "A (lambda (stream abort)) which will be called with keep-alive-stream-stream
   when the stream is either closed or we hit end of file or we hit end")))

(defun keep-alive-stream-close-underlying-stream (underlying-stream abort)
  (when (and underlying-stream (open-stream-p underlying-stream))
    (close underlying-stream :abort abort)))

(defclass keep-alive-chunked-stream (keep-alive-stream)
  ((chunked-stream :initarg :chunked-stream :accessor chunked-stream)))

(defun make-keep-alive-stream (stream &key end chunked-stream (on-close-or-eof #'keep-alive-stream-close-underlying-stream))
  "ON-CLOSE-OR-EOF takes a single parameter, STREAM (the stream passed in here, not the
keep-alive-stream), and should handle clean-up of it"
  (assert (xor end chunked-stream))
  (if chunked-stream
      (make-instance 'keep-alive-chunked-stream :stream stream :chunked-stream chunked-stream :on-close-or-eof on-close-or-eof)
      (make-instance 'keep-alive-stream :stream stream :end end :on-close-or-eof on-close-or-eof)))

(defun maybe-close (stream &optional close-if)
  "Will close the underlying stream if close-if is T (unless it is already closed).
   If the stream is already closed or we closed it returns :EOF otherwise NIL."
  (let ((underlying-stream (keep-alive-stream-stream stream)))
    (cond
      ((not underlying-stream)
       :eof)
      (close-if
       (funcall (close-action stream) underlying-stream nil)
       (setf (keep-alive-stream-stream stream) nil)
       :eof)
      (t nil))))

(defmethod stream-read-byte ((stream keep-alive-stream))
  "Return :EOF or byte read.  When we hit EOF or finish reading our allowed content,
   call the close-action on our underlying-stream and return EOF."
  (let ((byte :eof)
        (underlying-stream (keep-alive-stream-stream stream)))
    (or (maybe-close stream (<= (keep-alive-stream-end stream) 0))
        (progn
          (setf byte (read-byte underlying-stream nil :eof))
          (decf (keep-alive-stream-end stream) 1)
          (maybe-close stream (or (<= (keep-alive-stream-end stream) 0) (eql byte :eof)))
          byte))))

(defmethod stream-read-byte ((stream keep-alive-chunked-stream))
  "Return :EOF or byte read.  When we hit :EOF or finish reading our chunk,
   call the close-action on our underlying-stream and return :EOF"
  (or (maybe-close stream)
      (if (input-chunking-p (chunked-stream stream))
          (let ((byte (read-byte (chunked-stream stream) nil :eof)))
            (if (eql byte :eof)
                (prog1
                    byte
                  (maybe-close stream t))
                byte))
          (or (maybe-close stream t) :eof))))

(defmethod stream-read-sequence ((stream keep-alive-stream) sequence &optional start end)
  (declare (optimize speed))
  (let ((start (or start 0))
        (end (or end (length sequence))))
    (if (null (keep-alive-stream-stream stream)) ;; we already closed it
        start
        (let* ((to-read (min (- end start) (keep-alive-stream-end stream)))
               (n (read-sequence sequence (keep-alive-stream-stream stream)
                                 :start start
                                 :end (+ start to-read))))
          (decf (keep-alive-stream-end stream) (- n start))
          (maybe-close stream (<= (keep-alive-stream-end stream) 0))
          n))))

(defmethod stream-read-sequence ((stream keep-alive-chunked-stream) sequence &optional start end)
  (declare (optimize speed))
  (let ((start (or start 0))
        (end (or end (length sequence))))
    (if (null (keep-alive-stream-stream stream)) ;; we already closed it
        start
        (if (input-chunking-p (chunked-stream stream))
            (prog1
                (let ((num-read (read-sequence sequence (chunked-stream stream) :start start :end end)))
                  num-read)
              (maybe-close stream (not (input-chunking-p (chunked-stream stream)))))
            start))))

(defmethod stream-element-type ((stream keep-alive-chunked-stream))
  (stream-element-type (chunked-stream stream)))

(defmethod stream-element-type ((stream keep-alive-stream))
  '(unsigned-byte 8))

(defmethod open-stream-p ((stream keep-alive-stream))
  (let ((underlying-stream (keep-alive-stream-stream stream)))
    (and underlying-stream (open-stream-p underlying-stream))))

(defmethod close ((stream keep-alive-stream) &key abort)
  (funcall (close-action stream) (keep-alive-stream-stream stream) abort)
  (setf (keep-alive-stream-stream stream) nil))

;;; decoding-stream
(declaim (type fixnum +buffer-size+))
(eval-always (defconstant +buffer-size+ 128))
  
(defclass decoding-stream (fundamental-character-input-stream)
  ((stream :type decoding-stream
           :initarg :stream
           :initform (error ":stream is required")
           :accessor decoding-stream-of)
   (encoding :initarg :encoding
             :initform (error ":encoding is required")
             :accessor decoding-stream-encoding)
   (buffer :type (simple-array (unsigned-byte 8) (#.+buffer-size+))
           :initform (make-array +buffer-size+ :element-type '(unsigned-byte 8))
           :accessor decoding-stream-buffer)
   (buffer-position :type fixnum
                    :initform +buffer-size+
                    :accessor decoding-stream-buffer-position)
   (buffer-end-position :type fixnum
                        :initform -1
                        :accessor decoding-stream-buffer-end-position)
   (last-char :type character
              :initform #\Nul
              :accessor decoding-stream-last-char)
   (last-char-size :type fixnum
                   :initform 0
                   :accessor decoding-stream-last-char-size)
   (on-close :type (or null function) :initform nil :initarg :on-close)))

(defmethod initialize-instance :after ((stream decoding-stream) &rest initargs)
  (declare (ignore initargs))
  (with-slots (encoding) stream
    (when encoding
      (setf encoding (babel-encodings:get-character-encoding (sb-int:keywordicate encoding))))))

(defun make-decoding-stream (stream &key (encoding babel-encodings:*default-character-encoding*)
                                      (on-close))
  (let ((decoding-stream (make-instance 'decoding-stream
                                        :stream stream
                                        :encoding encoding
                                        :on-close on-close)))
    (dec-fill-buffer decoding-stream)
    decoding-stream))

(defun dec-fill-buffer (stream)
  (declare (optimize speed))
  (with-slots (stream buffer buffer-position buffer-end-position) stream
    (declare (type (simple-array (unsigned-byte 8) (#.+buffer-size+)) buffer)
             (type fixnum buffer-position))
    (let ((to-read (- +buffer-size+ buffer-position)))
      (declare (type fixnum to-read))
      (replace buffer buffer
               :start1 0
               :start2 buffer-position
               :end2 +buffer-size+)
      (setf buffer-position 0)
      (let ((n (read-sequence buffer stream :start to-read)))
        (declare (type fixnum n))
        (unless (= n +buffer-size+)
          (setf buffer-end-position n))))))

(defun needs-to-fill-buffer-p (stream)
  (declare (optimize speed))
  (when (/= -1 (the fixnum (decoding-stream-buffer-end-position stream)))
    (return-from needs-to-fill-buffer-p nil))
  (with-slots (buffer-position encoding) stream
    (< (- +buffer-size+ (the fixnum buffer-position))
       (the fixnum (babel-encodings:enc-max-units-per-char encoding)))))

(defmethod stream-read-char ((stream decoding-stream))
  (declare (optimize speed))
  (when (needs-to-fill-buffer-p stream)
    (dec-fill-buffer stream))

  (when (= (the fixnum (decoding-stream-buffer-end-position stream))
           (the fixnum (decoding-stream-buffer-position stream)))
    (return-from stream-read-char :eof))

  (with-slots (buffer buffer-position encoding last-char last-char-size)
      stream
    (declare (fixnum buffer-position))
    (let* ((mapping (babel-encodings:lookup-mapping babel::*string-vector-mappings* encoding))
           (counter (babel-encodings:code-point-counter mapping)))
      (declare (type function counter))
      (multiple-value-bind (chars new-end)
          (funcall counter buffer buffer-position +buffer-size+ 1)
        (declare (ignore chars) (fixnum new-end))
        (let ((string (make-string 1 :element-type 'babel:unicode-char))
              (size (the fixnum (- new-end buffer-position))))
          (funcall (the function (babel-encodings:decoder mapping))
                   buffer buffer-position new-end string 0)
          (setf buffer-position new-end
                last-char (aref string 0)
                last-char-size size)
          (aref string 0))))))

(defmethod stream-unread-char ((stream decoding-stream) char)
  (let ((last-char (decoding-stream-last-char stream)))
    (when (char= last-char #\Nul)
      (error "No character to unread from this stream"))
    (unless (char= char last-char)
      (error "Last character read (~S) was different from ~S"
             last-char char))
    (with-slots (buffer-position last-char-size) stream
      (decf buffer-position last-char-size))
    (with-slots (last-char last-char-size) stream
      (setf last-char #\Nul
            last-char-size 0))
    nil))

(defmethod open-stream-p ((stream decoding-stream))
  (open-stream-p (decoding-stream-of stream)))

(defmethod stream-element-type ((stream decoding-stream))
  'unicode-char)

(defmethod close ((stream decoding-stream) &key abort)
  ;; TODO: modify me to return the connection to the connection pool
  (with-slots (stream) stream
    (when (open-stream-p stream)
      (close stream :abort abort))))

;;; body
(defun decode-body (content-type body &key default-charset on-close)
  (let ((charset (or (and content-type
                          (detect-charset content-type body))
                     default-charset))
        (babel-encodings:*suppress-character-coding-errors* t))
    (if charset
        (handler-case
            (if (streamp body)
                (make-decoding-stream body :encoding charset :on-close on-close)
                (babel:octets-to-string body :encoding (keywordicate charset)))
          (babel:character-decoding-error (e)
            (warn (format nil "Failed to decode the body to ~S due to the following error (falling back to binary):~%  ~A"
                          charset
                          e))
            (return-from decode-body body)))
        body)))

(defun content-disposition (key val)
  (typecase val
    (cons (content-disposition key (first val)))
    (pathname
     (let* ((filename (file-namestring val))
            (utf8-filename-p (find-if (lambda (char)
                                        (< 127 (char-code char)))
                                      filename)))
       (format nil "Content-Disposition: form-data; name=\"~A\"; ~:[filename=\"~A\"~;filename*=UTF-8''~A~]~C~C"
               key
               utf8-filename-p
               (if utf8-filename-p
                   (obj/url:url-encode filename :encoding :utf-8)
                   filename)
               #\Return #\Newline)))
    (otherwise
      (format nil "Content-Disposition: form-data; name=\"~A\"~C~C"
              key
              #\Return #\Newline))))

(defmacro define-alist-cache (cache-name)
  (let ((var (intern (format nil "*~A*" cache-name))))
  `(progn
     (defvar ,var)
     (defun ,(intern (format nil "LOOKUP-IN-~A" cache-name)) (elt)
       (when (boundp ',var)
         (alexandria:assoc-value ,var elt)))
     (defun (setf ,(intern (format nil "LOOKUP-IN-~A" cache-name))) (val elt)
       (when (boundp ',var)
         (setf (alexandria:assoc-value ,var elt) val))
       val))))

;; If bound, an alist mapping content to content-type,
;; used to avoid determining content type multiple times
(define-alist-cache content-type-cache)
;; If bound, an alist mapping content to encoded content, to avoid
;; double converting content when we must calculate its length first
(define-alist-cache content-encoding-cache)

(defmacro with-content-caches (&body body)
  `(let ((*content-type-cache* nil)
         (*content-encoding-cache* nil))
     ,@body))

(defun content-type (value)
  (typecase value
    (pathname (or (lookup-in-content-type-cache value)
                  (setf (lookup-in-content-type-cache value) (mime value))))
    (otherwise nil)))

(defun multipart-value-content-type (value)
  (typecase value
    (cons
     (destructuring-bind (val &key content-type)
         value
       (or content-type (content-type val))))
    (otherwise (content-type value))))

(defun convert-to-octets (val)
  (or (lookup-in-content-encoding-cache val)
      (setf (lookup-in-content-encoding-cache val)
            (typecase val
              (string (babel:string-to-octets val))
              ((array (unsigned-byte 8) (*)) val)
              (symbol (babel:string-to-octets (princ-to-string val)))
              (cons (convert-to-octets (first val)))
              (otherwise (babel:string-to-octets (princ-to-string val)))))))

(defun write-as-octets (stream val)
  (typecase val
    ((array (unsigned-byte 8) (*)) (write-sequence val stream))
    (pathname
     (with-open-file (in val :element-type '(unsigned-byte 8))
       (alexandria:copy-stream in stream)))
    (string
     (write-sequence (convert-to-octets val) stream))
    (cons (write-as-octets stream (first val)))
    (otherwise (fast-write-sequence (convert-to-octets val) stream))))

(defun content-length (val)
  (typecase val
    (pathname (with-open-file (in val)
                (file-length in)))
    (cons (content-length (first val)))
    (otherwise (length (convert-to-octets val)))))

(defun multipart-content-length (content boundary)
  (declare (type simple-string boundary))
  (let ((boundary-length (length boundary)))
    (+ (loop for (key . val) in content
             sum (+ 2 ;; --
                    boundary-length
                    2 ;; CR LF
                    (length (the simple-string (content-disposition key val)))
                    (let ((content-type (multipart-value-content-type val)))
                      (if content-type
                          (+ #.(length "Content-Type: ") (length content-type) 2)
                          0))
                    2
                    (content-length val)
                    2)
               into total-length
             finally (return total-length))
       2 boundary-length 2 2)))

(defun write-multipart-content (content boundary stream)
  (let ((boundary (string-to-octets boundary)))
    (labels ((boundary-line (&optional endp)
               (fast-write-sequence (string-to-octets "--") stream)
               (fast-write-sequence boundary stream)
               (when endp
                 (fast-write-sequence (string-to-octets "--") stream))
               (crlf))
             (crlf () (fast-write-sequence +crlf+ stream)))
      (loop for (key . val) in content
            do (boundary-line)
               (fast-write-sequence (string-to-octets (content-disposition key val)) stream)
               (let ((content-type (multipart-value-content-type val)))
                 (when content-type
                   (fast-write-sequence
                     (string-to-octets
                       (format nil "Content-Type: ~A~C~C" content-type #\Return #\Newline))
                     stream)))
               (crlf)
               (write-as-octets stream val)
               (crlf)
            finally
               (boundary-line t)))))

(defun decompress-body (content-encoding body)
  (unless content-encoding
    (return-from decompress-body body))
  (cond
    ((string= content-encoding "gzip")
     (if (streamp body)
         (chipz:make-decompressing-stream :gzip body)
         (chipz:decompress nil (chipz:make-dstate :gzip) body)))
    ((string= content-encoding "deflate")
     (if (streamp body)
         (chipz:make-decompressing-stream :zlib body)
         (chipz:decompress nil (chipz:make-dstate :zlib) body)))
    ;; TODO 2024-10-20: 
    ((string= content-encoding "zstd")
     (if (streamp body)
         (io/flate:make-decompressing-stream :zstd body)
         (io/flate:decompress-with (make-instance 'io/zstd:zstd-decompressor) body)))
    (t body)))

;;; connection-cache
(defvar *use-connection-pool* t)
(defvar *max-active-connections* 8
  "Allowed number of active connections to all hosts.  If you change this,
  then call (make-new-connection-pool).")

(defstruct lru-pool-elt
  (prev nil :type (or null lru-pool-elt))
  (next nil :type (or null lru-pool-elt))
  (elt nil :type t)
  (key nil :type t)
  (eviction-callback nil :type (or null function)))

;; An LRU-POOL can have multiple entries for the same key
(defstruct lru-pool
  (lock #+sb-thread (sb-thread:make-mutex :name "connection pool lock")
        #-sb-thread nil)
  (hash-table nil :type (or null hash-table)) ;; hash table entries are lists of elements
  (head nil :type (or null lru-pool-elt)) ;; most recently used is here and it's a doubly-linked-list
  (tail nil :type (or null lru-pool-elt)) ;; least recently used is here
  (num-elts 0 :type fixnum)
  (max-elts 8 :type fixnum))

(defun make-connection-pool (&optional (max-active-connections *max-active-connections*))
  (make-lru-pool :hash-table (make-hash-table :test 'equal) :max-elts max-active-connections))

(defvar *connection-pool* nil)

(defun make-new-connection-pool (&optional (max-active-connections *max-active-connections*))
  (clear-connection-pool)
  (setf *connection-pool* (make-connection-pool max-active-connections)))

(defun get-from-lru-pool (lru-pool key)
  "Takes an element from the LRU-POOL matching KEY.  Must be called with LRU-POOL-LOCK held.
   The element is removed from the pool."
  (let* ((hash-table (lru-pool-hash-table lru-pool))
         (possible-elts (gethash key (lru-pool-hash-table lru-pool))))
    (when possible-elts
      (let ((remaining-elts (cdr possible-elts)))
        (if remaining-elts
            (setf (gethash key hash-table) remaining-elts)
            (remhash key hash-table)))
      (let ((elt (car possible-elts)))
        (let ((prev (lru-pool-elt-prev elt))
              (next (lru-pool-elt-next elt)))
          (if prev
              (setf (lru-pool-elt-next prev) next)
              (setf (lru-pool-head lru-pool) next))
          (if next
              (setf (lru-pool-elt-prev next) prev)
              (setf (lru-pool-tail lru-pool) prev)))
        (decf (lru-pool-num-elts lru-pool))
        (lru-pool-elt-elt elt)))))

(defun evict-tail (lru-pool)
  "Removes the least recently used element of the LRU-POOL and returns
    (values evicted-element eviction-callback t) if there was
   an element to remove, otherwise nil.  Must be called with LRU-POOL-LOCK held.

   Outside the LRU-POOL-LOCK you must call the returned EVICTION-CALLBACK with the EVICTED-ELEMENT."
  ;; slightly different from get-from-lru-pool because we want to get rid of the
  ;; actual oldest element (one could in principle call get-from-lru-pool on
  ;; (lru-pool-elt-key (lru-pool-tail lru-pool)) if you didn't care
  (let* ((tail (lru-pool-tail lru-pool)))
    (when tail
      (let ((prev (lru-pool-elt-prev tail)))
        (if prev
            (setf (lru-pool-elt-next prev) nil)
            (setf (lru-pool-head lru-pool) nil))
        (setf (lru-pool-tail lru-pool) prev)
        (let* ((hash-table (lru-pool-hash-table lru-pool))
               (key (lru-pool-elt-key tail))
               (remaining (cl:delete tail (gethash key hash-table))))
          (if remaining
              (setf (gethash key hash-table) remaining)
              (remhash key hash-table))))
      (decf (lru-pool-num-elts lru-pool))
      (values (lru-pool-elt-elt tail) (lru-pool-elt-eviction-callback tail) t))))

(defun add-to-lru-pool (lru-pool key elt eviction-callback)
  "Adds ELT to an LRU-POOL with potentially non-unique KEY, potentially evicting another element to
   make room.  EVICTION-CALLBACK will be called with one parameter ELT, when ELT is evicted from the
   LRU-POOL.  ADD-TO-LRU-POOL must be called with LRU-POOL-LOCK held.

   If an element was evicted to make space, returns (values evicted-elt eviction-callback t)
   otherwise nil.  The EVICTION-CALLBACK should take one parameter, the evicted element."
  (declare (type lru-pool lru-pool))
  (let* ((old-head (lru-pool-head lru-pool))
         (lru-pool-elt (make-lru-pool-elt :prev nil :next old-head :elt elt :key key :eviction-callback eviction-callback))
         (hash-table (lru-pool-hash-table lru-pool)))
    (setf (lru-pool-head lru-pool) lru-pool-elt)
    (push lru-pool-elt (gethash key hash-table))
    (when old-head
      (setf (lru-pool-elt-prev old-head) lru-pool-elt))
    (unless (lru-pool-tail lru-pool)
      (setf (lru-pool-tail lru-pool) lru-pool-elt))
    (when (> (incf (lru-pool-num-elts lru-pool)) (lru-pool-max-elts lru-pool))
      (evict-tail lru-pool))))

(defmethod print-object ((obj lru-pool-elt) str) ;; avoid printing loops
  (print-unreadable-object (obj str :type "LRU-POOL-ELT")
    (format str "~A NEXT ~A" (lru-pool-elt-key obj) (lru-pool-elt-next obj))))

(defmethod print-object ((obj lru-pool) str) ;; avoid printing loops
  (print-unreadable-object (obj str :type "LRU-POOL")
    (let (objs)
      (loop with lru-pool-elt = (lru-pool-head obj)
            while lru-pool-elt
            do (push (list (lru-pool-elt-key lru-pool-elt) (lru-pool-elt-elt lru-pool-elt)) objs)
            do (setf lru-pool-elt (lru-pool-elt-next lru-pool-elt)))
      (if objs
          (format str "~A/~A elts~%~{ ~{~A~^: ~}~^~%~}" (lru-pool-num-elts obj) (lru-pool-max-elts obj) objs)
          (format str "empty")))))

(defmacro with-lock (lock &body body)
  #+thread-support `(sb-thread:with-mutex (,lock)
                      ,@body)
  #-thread-support `(progn ,@body))

(defun push-connection (host-port stream &optional eviction-callback)
  "Add STREAM back to connection pool with key HOST-PORT.  EVICTION-CALLBACK
   must be a function of a single parameter, and will be called with STREAM
   if the HOST-PORT/SOCKET pair is evicted from the connection pool."
  (when *use-connection-pool*
    (let ((pool *connection-pool*))
      (multiple-value-bind (evicted-elt eviction-callback)
          (with-lock (lru-pool-lock pool)
            (add-to-lru-pool pool host-port stream eviction-callback))
        (and eviction-callback (funcall eviction-callback evicted-elt))
        (values)))))

(defun steal-connection (host-port)
  "Return the STREAM associated with key HOST-PORT"
  (when *use-connection-pool*
    (let ((pool *connection-pool*))
      (with-lock (lru-pool-lock pool)
        (get-from-lru-pool pool host-port)))))

(defun clear-connection-pool ()
  "Remove all elements from the connection pool, calling their eviction-callbacks."
  (when *use-connection-pool*
    (let ((pool *connection-pool*)
          evicted-element eviction-callback element-was-evicted)
      (when pool
        (loop for count from 0
              do (setf (values evicted-element eviction-callback element-was-evicted)
                       (with-lock (lru-pool-lock pool)
                         (evict-tail pool)))
              do (when eviction-callback (funcall eviction-callback evicted-element))
              while element-was-evicted)))))

(make-new-connection-pool)

;;; backend
(eval-always
  (defparameter *ca-bundle*
    #.(uiop:native-namestring #P"/etc/ca-certificates/extracted/ca-bundle.trust.crt")
    "The default public root certificates used in requests."))
   

(defun read-until-crlf*2 (stream)
  (with-fast-output (buf)
    (tagbody
     read-cr
       (loop for byte of-type (or (unsigned-byte 8) null) = (read-byte stream nil nil)
             if byte
               do (fast-write-byte byte buf)
             else
               do (go eof)
             until (= byte (char-code #\Return)))

     read-lf
       (let ((next-byte (read-byte stream nil nil)))
         (unless next-byte
           (go eof))
         (locally (declare (type (unsigned-byte 8) next-byte))
           (cond
             ((= next-byte (char-code #\Newline))
              (fast-write-byte next-byte buf)
              (go read-cr2))
             ((= next-byte (char-code #\Return))
              (fast-write-byte next-byte buf)
              (go read-lf))
             (T
              (fast-write-byte next-byte buf)
              (go read-cr)))))

     read-cr2
       (let ((next-byte (read-byte stream nil nil)))
         (unless next-byte
           (go eof))
         (locally (declare (type (unsigned-byte 8) next-byte))
           (cond
             ((= next-byte (char-code #\Return))
              (fast-write-byte next-byte buf)
              (go read-lf2))
             (T
              (fast-write-byte next-byte buf)
              (go read-cr)))))

     read-lf2
       (let ((next-byte (read-byte stream nil nil)))
         (unless next-byte
           (go eof))
         (locally (declare (type (unsigned-byte 8) next-byte))
           (cond
             ((= next-byte (char-code #\Newline))
              (fast-write-byte next-byte buf))
             ((= next-byte (char-code #\Return))
              (fast-write-byte next-byte buf)
              (go read-lf))
             (T
              (fast-write-byte next-byte buf)
              (go read-cr)))))

     eof)))

(defvar *empty-body*
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun read-response (stream has-body collect-headers read-body)
  (let* ((http (make-http-response))
         body
         body-data
         (headers-data (and collect-headers
                            (make-output-buffer)))
         (header-finished-p nil)
         (finishedp nil)
         (content-length nil)
         (transfer-encoding-p)
         (parser (make-http-parser http
                              :header-callback
                              (lambda (headers)
                                (setq header-finished-p t
                                      content-length (gethash "content-length" headers)
                                      transfer-encoding-p (gethash "transfer-encoding" headers))
                                (unless (and has-body
                                             (or content-length
                                                 transfer-encoding-p))
                                  (setq finishedp t)))
                              :body-callback
                              (lambda (data start end)
                                (when body-data
                                  (fast-write-sequence data body-data start end)))
                              :finish-callback
                              (lambda ()
                                (setq finishedp t)))))
    (let ((buf (read-until-crlf*2 stream)))
      (declare (type octet-vector buf))
      (when collect-headers
        (fast-write-sequence buf headers-data))
      (funcall parser buf))
    (unless header-finished-p
      (error "maybe invalid header"))
    (cond
      ((not read-body)
       (setq body stream))
      ((not has-body)
       (setq body *empty-body*))
      ((and content-length (not transfer-encoding-p))
       (let ((buf (make-array (etypecase content-length
                                (integer content-length)
                                (string (parse-integer content-length)))
                              :element-type '(unsigned-byte 8))))
         (read-sequence buf stream)
         (setq body buf)))
      ((let ((status (http-status http)))
         (or (= status 100)    ;; Continue
             (= status 101)    ;; Switching Protocols
             (= status 204)    ;; No Content
             (= status 304))) ;; Not Modified
       (setq body *empty-body*))
      (T
       (setq body-data (make-output-buffer))
       (loop for buf of-type octet-vector = (read-until-crlf*2 stream)
             do (funcall parser buf)
             until (or finishedp
                       (zerop (length buf)))
             finally
                (setq body (finish-output-buffer body-data)))))
    (values http
            body
            (and collect-headers
                 (finish-output-buffer headers-data))
            transfer-encoding-p)))

(defun print-verbose-data (direction &rest data)
  (flet ((boundary-line ()
           (let ((char (ecase direction
                         (:incoming #\<)
                         (:outgoing #\>))))
             (fresh-line)
             (dotimes (i 50)
               (write-char char))
             (fresh-line))))
    (boundary-line)
    (dolist (d data)
      (map nil (lambda (byte)
                 (princ (code-char byte)))
           d))
    (boundary-line)))

(defun convert-body (body content-encoding content-type content-length chunkedp force-binary force-string keep-alive-p on-close)
  (when (streamp body)
    (cond
      ((and keep-alive-p chunkedp)
       (setf body (make-keep-alive-stream body :chunked-stream
                                          (let ((chunked-stream (make-chunked-stream body)))
                                            (setf (input-chunking-p chunked-stream) t)
                                            chunked-stream) 
                                               :on-close-or-eof on-close)))
      ((and keep-alive-p content-length)
       (setf body (make-keep-alive-stream body :end content-length :on-close-or-eof on-close)))
      (chunkedp
       (let ((chunked-stream (make-chunked-stream body)))
         (setf (input-chunking-p chunked-stream) t)
         (setf body chunked-stream)))))
  (let ((body (decompress-body content-encoding body)))
    (if force-binary
        body
        (decode-body content-type body
                     :default-charset (if force-string
                                          babel:*default-character-encoding*
                                          nil)))))

(defun build-cookie-headers (uri cookie-jar)
  (with-header-output (buffer)
    (let ((cookies (cookie-jar-host-cookies cookie-jar (uri-host uri) (or (uri-path uri) "/")
                                            :securep (string= (uri-scheme uri) "https"))))
      (when cookies
        (fast-write-sequence (string-to-octets "Cookie: ") buffer)
        (fast-write-sequence
         (string-to-octets (write-cookie-header cookies))
         buffer)
        (fast-write-sequence +crlf+ buffer)))))

(defun make-connect-stream (uri version stream &optional proxy-auth)
  (let ((header (with-fast-output (buffer)
                  (write-connect-header uri version buffer proxy-auth))))
    (write-sequence header stream)
    (force-output stream)
    (read-until-crlf*2 stream)
    stream))

(defun make-proxy-authorization (uri)
  (let ((proxy-auth (obj/uri:uri-userinfo uri)))
    (when proxy-auth
      (format nil "Basic ~A"
              (dat/base64:string-to-base64-string proxy-auth)))))

(defconstant +socks5-version+ 5)
(defconstant +socks5-reserved+ 0)
(defconstant +socks5-no-auth+ 0)
(defconstant +socks5-connect+ 1)
(defconstant +socks5-domainname+ 3)
(defconstant +socks5-succeeded+ 0)
(defconstant +socks5-ipv4+ 1)
(defconstant +socks5-ipv6+ 4)

(defun ensure-socks5-connected (input output uri http-method)
  (labels ((fail (condition &key reason)
             (error (make-condition condition
                                    :body nil :status nil :headers nil
                                    :uri uri
                                    :method http-method
                                    :reason reason)))
           (exact (n reason)
             (unless (eql n (read-byte input nil 'eof))
               (fail 'socks5-proxy-request-failed :reason reason)))
           (drop (n reason)
             (dotimes (i n)
               (when (eq (read-byte input nil 'eof) 'eof)
                 (fail 'socks5-proxy-request-failed :reason reason)))))
    ;; Send Version + Auth Method
    ;; Currently, only supports no-auth method.
    (write-byte +socks5-version+ output)
    (write-byte 1 output)
    (write-byte +socks5-no-auth+ output)
    (finish-output output)

    ;; Receive Auth Method
    (exact +socks5-version+ "Unexpected version")
    (exact +socks5-no-auth+ "Unsupported auth method")

    ;; Send domainname Request
    (let* ((host (babel:string-to-octets (uri-host uri)))
           (hostlen (length host))
           (port (uri-port uri)))
      (unless (<= 1 hostlen 255)
        (fail 'socks5-proxy-request-failed :reason "domainname too long"))
      (unless (<= 1 port 65535)
        (fail 'socks5-proxy-request-failed :reason "Invalid port"))
      (write-byte +socks5-version+ output)
      (write-byte +socks5-connect+ output)
      (write-byte +socks5-reserved+ output)
      (write-byte +socks5-domainname+ output)
      (write-byte hostlen output)
      (write-sequence host output)
      (write-byte (ldb (byte 8 8) port) output)
      (write-byte (ldb (byte 8 0) port) output)
      (finish-output output)

      ;; Receive reply
      (exact +socks5-version+ "Unexpected version")
      (exact +socks5-succeeded+ "Unexpected result code")
      (drop 1 "Should be reserved byte")
      (let ((atyp (read-byte input nil 'eof)))
        (cond
          ((eql atyp +socks5-ipv4+)
           (drop 6 "Should be IPv4 address and port"))
          ((eql atyp +socks5-ipv6+)
           (drop 18 "Should be IPv6 address and port"))
          ((eql atyp +socks5-domainname+)
           (let ((n (read-byte input nil 'eof)))
             (when (eq n 'eof)
               (fail 'socks5-proxy-request-failed :reason "Invalid domainname length"))
             (drop n "Should be domainname and port")))
          (t
           (fail 'socks5-proxy-request-failed :reason "Unknown address")))))))

(defun make-ssl-stream (stream ca-path ssl-key-file ssl-cert-file ssl-key-password hostname insecure)
  (progn
    (cl+ssl:ensure-initialized)
    (let ((ctx (cl+ssl:make-context :verify-mode
                                    (if insecure
                                        cl+ssl:+ssl-verify-none+
                                        cl+ssl:+ssl-verify-peer+)
                                    :verify-location
                                    ;; TODO 2024-05-22: 
                                    (cond
                                      (ca-path (uiop:native-namestring ca-path))
                                      ((probe-file *ca-bundle*) *ca-bundle*)
                                      ;; In executable environment, perhaps *ca-bundle* doesn't exist.
                                      (t :default))))
          (ssl-cert-pem-p (and ssl-cert-file
                               (std/seq:ends-with-subseq ".crt" ssl-cert-file))))
      (cl+ssl:with-global-context (ctx :auto-free-p t)
        (when ssl-cert-pem-p
          (cl+ssl:use-certificate-chain-file ssl-cert-file))
        (cl+ssl:make-ssl-client-stream stream
                                       :hostname hostname
                                       :verify (not insecure)
                                       :key ssl-key-file
                                       :certificate (and (not ssl-cert-pem-p)
                                                         ssl-cert-file)
                                       :password ssl-key-password)))))

(defstruct %wrapped-stream
  stream)

;; Forward methods the user might want to use on this.
;; User is not meant to interact with this object except
;; potentially to close it when they decide they don't
;; need the :keep-alive connection anymore.
(defmethod close ((u %wrapped-stream) &key abort)
  (close (%wrapped-stream-stream u) :abort abort))

(defmethod open-stream-p ((u %wrapped-stream))
  (open-stream-p (%wrapped-stream-stream u)))

(defun request (uri &rest args
                            &key (method :get) (version 1.1)
                                 content headers
                                 basic-auth bearer-auth
                                 cookie-jar
                                 (connect-timeout *default-connect-timeout*)
                                 (read-timeout *default-read-timeout*)
                                 (keep-alive t) (use-connection-pool t)
                                 (max-redirects 5)
                                 ssl-key-file ssl-cert-file ssl-key-password
                                 stream (verbose *verbose*)
                                 force-binary
                                 force-string
                                 want-stream
                                 (proxy *default-proxy*)
                                 (insecure *no-ssl*)
                                 ca-path
                    &aux
                    (proxy-uri (and proxy (obj/uri:uri proxy)))
                    (original-user-supplied-stream stream)
                    (user-supplied-stream (if (%wrapped-stream-p stream) (%wrapped-stream-stream stream) stream)))
  (declare (ignorable ssl-key-file ssl-cert-file ssl-key-password
                      connect-timeout read-timeout)
           (type real version)
           (type fixnum max-redirects))
  (with-content-caches
  (labels ((make-new-connection (uri)
             (restart-case
                 (let* ((con-uri (uri (or proxy uri)))
                        (socket (make-instance 'sb-bsd-sockets:inet-socket
                                  :type :stream
                                  :protocol :tcp))
                        (connection (sb-bsd-sockets:socket-connect
                                     socket
                                     (sb-bsd-sockets:make-inet-address (net/proto/dns:resolve (uri-host con-uri)))
                                     (or (uri-port con-uri) (when insecure 80) 443)))
                        (stream (sb-bsd-sockets:socket-make-stream connection
                                                                   :input t
                                                                   :output t
                                                                   :timeout connect-timeout
                                                                   :auto-close t
                                                                   :element-type :default))
                          
                        (scheme (uri-scheme uri)))
                   (declare (type keyword scheme))
                   ;; (when read-timeout ;; TODO 2024-06-19: test
                   ;;   (setf (io/socket:sockopt-receive-timeout connection) read-timeout)) 
                   (when (socks5-proxy-p proxy-uri)
                     (ensure-socks5-connected stream stream uri method))
                   (if (string= (symbol-name scheme) "HTTPS")
                       (make-ssl-stream (if (http-proxy-p proxy-uri)
                                               (make-connect-stream uri version stream (make-proxy-authorization con-uri))
                                               stream) ca-path ssl-key-file ssl-cert-file ssl-key-password (uri-host uri) insecure)
                       stream))
               (retry-request ()
                 :report "Retry the same request."
                 (return-from request
                   (apply #'request uri :use-connection-pool nil args)))
               (retry-insecure ()
                 :report "Retry the same request without checking for SSL certificate validity."
                 (return-from request
                   (apply #'request uri :use-connection-pool nil :insecure t args)))))
           (http-proxy-p (uri)
             (and uri
                  (let ((scheme (uri-scheme uri)))
                    (and (stringp scheme)
                         (or (string= scheme "http")
                             (string= scheme "https"))))))
           (socks5-proxy-p (uri)
             (and uri
                  (let ((scheme (uri-scheme uri)))
                    (and (stringp scheme)
                         (string= scheme "socks5")))))
           (connection-keep-alive-p (connection-header)
             (and keep-alive
                  (or (and (= (the real version) 1.0)
                           (equalp connection-header "keep-alive"))
                      (not (equalp connection-header "close")))))
           (return-stream-to-pool (stream uri)
             (push-connection (format nil "~A://~A"
                                      (uri-scheme uri)
                                      (uri-authority uri)) stream #'close))
           (return-stream-to-pool-or-close (stream connection-header uri)
             (if (and (not user-supplied-stream) use-connection-pool (connection-keep-alive-p connection-header))
                 (return-stream-to-pool stream uri)
                 (when (open-stream-p stream)
                   (close stream))))
           (finalize-connection (stream connection-header uri)
             "If KEEP-ALIVE is in the connection-header and the user is not requesting a stream,
              we will push the connection to our connection pool if allowed, otherwise we return
              the stream back to the user who must close it."
             (unless want-stream
               (cond
                 ((and use-connection-pool (connection-keep-alive-p connection-header) (not user-supplied-stream))
                   (return-stream-to-pool stream uri))
                 ((not (connection-keep-alive-p connection-header))
                  (when (open-stream-p stream)
                    (close stream)))))))
    (let* ((uri (uri uri))
           (proxy (when (http-proxy-p proxy-uri) proxy))
           (content-type (cdr (find :content-type headers :key #'car :test #'string-equal)))
           (multipart-p (or (and content-type
                                 (>= (length content-type) 10)
				 (string= content-type "multipart/" :end1 10))
                            (and (not content-type)
                                 (consp content)
                                 (find-if #'pathnamep content :key #'cdr))))
           (form-urlencoded-p (or (string= content-type "application/x-www-form-urlencoded")
                                  (and (not content-type)
                                       (consp content)
                                       (not multipart-p))))
           (boundary (and multipart-p
                          (make-random-string 12)))
           (content (if (and form-urlencoded-p (not (stringp content))) ;; user can provide already encoded content, trust them.
                        (obj/url::url-encode-params content)
                        content))
           (stream (or user-supplied-stream
                       (and use-connection-pool
                            (steal-connection (format nil "~A://~A"
                                                      (uri-scheme uri)
                                                      (uri-authority uri))))))
           (reusing-stream-p (not (null stream))) ;; user provided or from connection-pool
           (stream (or stream
                       (make-new-connection uri)))
           (content-length
             (assoc :content-length headers :test #'string-equal))
           (transfer-encoding
             (assoc :transfer-encoding headers :test #'string-equal))
           (chunkedp (or (and transfer-encoding
                              (equalp (cdr transfer-encoding) "chunked"))
                         (and content-length
                              (null (cdr content-length)))))
           (first-line-data
             (with-fast-output (buffer)
               (write-first-line method uri version buffer)))
           (headers-data
             (flet ((write-header* (name value)
                      (let ((header (assoc name headers :test #'string-equal)))
                        (if header
                            (when (cdr header)
                              (write-header name (cdr header)))
                            (write-header name value)))
                      (values)))
               (with-header-output (buffer)
                 (write-header* :user-agent #.*default-user-agent*)
                 (write-header* :host (uri-authority uri))
                 (write-header* :accept "*/*")
                 (cond
                   ((and keep-alive
                         (= (the real version) 1.0))
                    (write-header* :connection "keep-alive"))
                   ((and (not keep-alive)
                         (= (the real version) 1.1))
                    (write-header* :connection "close")))
		 (cond ((and bearer-auth basic-auth)
			(error "You should only use one Authorization header."))
		       (basic-auth
			(write-header* :authorization
				       (format nil "Basic ~A"
					       (dat/base64::string-to-base64-string
						(format nil "~A:~A"
							(car basic-auth)
							(cdr basic-auth))))))
		       (bearer-auth
			(write-header* :authorization
				       (format nil "Bearer ~A" bearer-auth))))
                 (when proxy
                   (let ((scheme (uri-scheme uri)))
                     (when (string= scheme "http")
                       (let* ((uri (uri proxy))
                              (proxy-authorization (make-proxy-authorization uri)))
                         (when proxy-authorization
                           (write-header* :proxy-authorization proxy-authorization))))))
                 (cond
                   (multipart-p
                    (write-header :content-type (format nil "~A; boundary=~A"
                                                        (or content-type "multipart/form-data")
                                                        boundary))
                    (unless chunkedp
                      (write-header :content-length
                                    (multipart-content-length content boundary))))
                   (form-urlencoded-p
                    (write-header* :content-type "application/x-www-form-urlencoded")
                    (unless chunkedp
                      (write-header* :content-length (length (the string content)))))
                   (t
                    (etypecase content
                      (null
                       (unless chunkedp
                         (write-header* :content-length 0)))
                      (string
                       (write-header* :content-type (or content-type "text/plain"))
                       (unless chunkedp
                         (write-header* :content-length (content-length content))))
                      ((array (unsigned-byte 8) *)
                       (write-header* :content-type (or content-type "text/plain"))
                       (unless chunkedp
                         (write-header* :content-length (length content))))
                      (pathname
                       (write-header* :content-type (or content-type (content-type content)))
                       (unless chunkedp
                         (write-header :content-length
                                       (or (cdr (assoc :content-length headers :test #'string-equal))
                                           (content-length content))))))))
                 ;; Transfer-Encoding: chunked
                 (when (and chunkedp
                            (not transfer-encoding))
                   (write-header* :transfer-encoding "chunked"))

                 ;; Custom headers
                 (loop for (name . value) in headers
                       unless (member name '(:user-agent :host :accept
                                             :connection
                                             :content-type :content-length) :test #'string-equal)
                         do (write-header name value)))))
           (cookie-headers (and cookie-jar
                                (build-cookie-headers uri cookie-jar))))
      (macrolet ((maybe-try-again-without-reusing-stream (&optional (force nil))
                   `(progn ;; retrying by go retry avoids generating the header, parsing, etc.
                      (when (open-stream-p stream)
                        (close stream :abort t)
                        (setf stream nil))
                      
                      (when ,(or force 'reusing-stream-p)
                        (setf reusing-stream-p nil
                              user-supplied-stream nil
                              stream (make-new-connection uri))
                        (go retry))))
                 (try-again-without-reusing-stream ()
                   `(maybe-try-again-without-reusing-stream t))
                 (with-retrying (&body body)
                   `(restart-case
                        (handler-bind (((and error
                                             ;; We should not retry errors received from the server.
                                             ;; Only technical errors such as disconnection or some
                                             ;; problems with the protocol should be retried automatically.
                                             ;; This solves https://github.com/fukamachi/dexador/issues/137 issue.
                                             (not http-request-failed))
                                         (lambda (e)
                                           (declare (ignorable e))
                                           (maybe-try-again-without-reusing-stream))))
                          ,@body)
                      (retry-request () :report "Retry the same request."
                        (return-from request (apply #'request uri args)))
                      (ignore-and-continue () :report "Ignore the error and continue."))))
        (tagbody
         retry

           (unless (open-stream-p stream)
             (try-again-without-reusing-stream))
           
           (with-retrying
             (write-sequence first-line-data stream)
             (write-sequence headers-data stream)
             (when cookie-headers
               (write-sequence cookie-headers stream))
             (write-sequence +crlf+ stream)
             (force-output stream))

           ;; Sending the content
           (when content
             (let ((stream (if chunkedp
                               (make-chunked-stream stream)
                               stream)))
               (when chunkedp
                 (setf (output-chunking-p stream) t))
               (with-retrying
                 (if (consp content)
                     (write-multipart-content content boundary stream)
                     (write-as-octets stream content))
                 (when chunkedp
                   (setf (output-chunking-p stream) nil))
                 (finish-output stream))))

         start-reading
           (multiple-value-bind (http body response-headers-data transfer-encoding-p)
               (with-retrying
                   (read-response stream (not (eq method :head)) verbose (not want-stream)))
             (let* ((status (http-status http))
                    (response-headers (http-headers http))
                    (content-length (gethash "content-length" response-headers))
                    (content-length (etypecase content-length
                                      (null content-length)
                                      (string (parse-integer content-length))
                                      (integer content-length))))
               (when (= status 0)
                 (with-retrying
                   (http-request-failed status
                                        :body body
                                        :headers headers
                                        :uri uri
                                        :method method)))
               (when verbose
                 (print-verbose-data :outgoing first-line-data headers-data cookie-headers +crlf+)
                 (print-verbose-data :incoming response-headers-data))
               (when cookie-jar
                 (when-let ((set-cookies (append (gethash "set-cookie" response-headers)
                                                (ensure-list (gethash "set-cookie2" response-headers)))))
                   (net/cookie::merge-cookies cookie-jar
                                  (remove nil (mapcar (lambda (cookie)
                                                        (declare (type string cookie))
                                                        (unless (= (length cookie) 0)
                                                          (net/cookie:parse-set-cookie-header cookie
                                                                                   (uri-host uri)
                                                                                   (uri-path uri))))
                                                      set-cookies)))))
               (when (and (member status '(301 302 303 307 308) :test #'=)
                          (gethash "location" response-headers)
                          (/= max-redirects 0))
                 ;; Need to read the response body
                 (when (and want-stream
                            (not (eq method :head)))
                   (cond
                     ((integerp content-length)
                      (dotimes (i content-length)
                        (loop until (read-byte body nil nil))))
                     (transfer-encoding-p
                       (read-until-crlf*2 body))))

                 (let* ((location-uri (uri (gethash "location" response-headers)))
                        (same-server-p (or (null (uri-host location-uri))
                                           (and (string= (uri-scheme location-uri)
                                                         (uri-scheme uri))
                                                (string= (uri-host location-uri)
                                                         (uri-host uri))
                                                (eql (uri-port location-uri)
                                                     (uri-port uri))))))
                   (if (and same-server-p
                            (or (= status 307) (= status 308)
                                (member method '(:get :head) :test #'eq)))
                       (progn ;; redirection to the same host
                         (setq uri (merge-uris location-uri uri))
                         (setq first-line-data
                               (with-fast-output (buffer)
                                 (write-first-line method uri version buffer)))
                         (when cookie-jar
                           ;; Rebuild cookie-headers.
                           (setq cookie-headers (build-cookie-headers uri cookie-jar)))
                         (decf max-redirects)
                         (if (equalp (gethash "connection" response-headers) "close")
                             (try-again-without-reusing-stream)
                             (progn
                               (setq reusing-stream-p t)
                               (go retry))))
                       (progn ;; this is a redirection to a different host
                         (setf location-uri (merge-uris location-uri uri))
                         ;; Close connection if it isn't from our connection pool or from the user and we aren't going to
                         ;; pass it to our new call.
                         (when (not same-server-p) (return-stream-to-pool-or-close stream (gethash "connection" response-headers) uri))
                         (setf (getf args :headers)
                               (nconc `((:host . ,(uri-host location-uri))) headers))
                         (setf (getf args :max-redirects)
                               (1- max-redirects))
                         ;; Redirect as GET if it's 301, 302, 303
                         (unless (or (= status 307) (= status 308)
                                     (member method '(:get :head) :test #'eq))
                           (setf (getf args :method) :get))
                         (return-from request
                           (apply #'request location-uri (if same-server-p
                                                             args
                                                             (progn (remf args :stream) args))))))))
               (unwind-protect
                    (let* ((keep-connection-alive (connection-keep-alive-p
                                                   (gethash "connection" response-headers)))
                           (body (convert-body body
                                              (gethash "content-encoding" response-headers)
                                              (gethash "content-type" response-headers)
                                              content-length
                                              transfer-encoding-p
                                              force-binary
                                              force-string
                                              keep-connection-alive
                                              (if (and use-connection-pool keep-connection-alive (not user-supplied-stream) (streamp body))
                                                  (lambda (underlying-stream abort)
                                                    (declare (ignore abort))
                                                    (when (and underlying-stream (open-stream-p underlying-stream))
                                                      ;; read any left overs the user may have not read (in case of errors on user side?)
                                                      (loop while (ignore-errors (listen underlying-stream)) ;; ssl streams may close
                                                            do (read-byte underlying-stream nil nil))
                                                      (when (open-stream-p underlying-stream)
                                                        (push-connection (format nil "~A://~A"
                                                                                 (uri-scheme uri)
                                                                                 (uri-authority uri)) underlying-stream #'close))))
                                                  #'keep-alive-stream-close-underlying-stream))))
                      ;; Raise an error when the HTTP response status code is 4xx or 50x.
                      (when (<= 400 status)
                        (with-retrying
                          (http-request-failed status
                                               :body body
                                               :headers response-headers
                                               :uri uri
                                               :method method)))
                      ;; Have to be a little careful with the fifth value stream we return --
                      ;; the user may be not aware that keep-alive t without use-connection-pool can leak
                      ;; sockets, so we wrap the returned last value so when it is garbage
                      ;; collected it gets closed.  If the user is getting a stream back as BODY,
                      ;; then we instead add a finalizer to that stream to close it when garbage collected
                      (return-from request
                        (values body
                                status
                                response-headers
                                uri
                                (when (and keep-alive
                                           (not (equalp (gethash "connection" response-headers) "close"))
                                           (or (not use-connection-pool) user-supplied-stream))
                                  (or (and original-user-supplied-stream ;; user provided a stream
					   (if (%wrapped-stream-p original-user-supplied-stream) ;; but, it came from us
					       (eql (%wrapped-stream-stream original-user-supplied-stream) stream) ;; and we used it
					       (eql original-user-supplied-stream stream)) ;; user provided a bare stream
					   original-user-supplied-stream) ;; return what the user sent without wrapping it
                                      (if want-stream ;; add a finalizer to the body to close the stream
                                          (progn
                                            (sb-ext:finalize body (lambda () (close stream)))
                                            stream)
                                          (let ((wrapped-stream (make-%wrapped-stream :stream stream)))
                                            (sb-ext:finalize wrapped-stream (lambda () (close stream)))
                                            wrapped-stream)))))))
                 (finalize-connection stream (gethash "connection" response-headers) uri))))))))))

;;; API
(defun get (uri &rest args
            &key version headers basic-auth bearer-auth cookie-jar keep-alive use-connection-pool
	      connect-timeout read-timeout max-redirects
	      force-binary force-string want-stream content
              ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  "Make a GET request to URI and return
    (values body-or-stream status response-headers uri &optional opaque-socket-stream)

  You may pass a real stream in as STREAM if you want us to communicate with the server via it --
  though if any errors occur, we will open a new connection to the server.  If you have a previous
  OPAQUE-SOCKET-STREAM you can pass that in as STREAM as well and we will re-use that connection.

  OPAQUE-SOCKET-STREAM is not returned if USE-CONNECTION-POOL is T, instead we keep track of it and
  re-use it when needed.

  If WANT-STREAM is T, then a STREAM is returned as the first value.  You may read this as needed to
  get the body of the response.  If KEEP-ALIVE and USE-CONNECTION-POOL are T, then the stream will be
  returned to the connection pool when you have read all the data or closed the stream. If KEEP-ALIVE
  is NIL then you are responsible for closing the stream when done.

  If KEEP-ALIVE is T and USE-CONNECTION-POOL is NIL, then the fifth value returned is a stream which
  you can then pass in again using the STREAM option to re-use the active connection.  If you ignore
  the stream, it will get closed during garbage collection.

  If KEEP-ALIVE is T and USE-CONNECTION-POOL is T, then there is no fifth
  value (OPAQUE-SOCKET-STREAM) returned, but the active connection to the host/port may be reused in
  subsequent calls.  This removes the need for the caller to keep track of the active socket-stream
  for subsequent calls.

  While CONTENT is allowed in a GET request the results are ill-defined and not advised."
  (declare (ignore version headers basic-auth bearer-auth cookie-jar keep-alive use-connection-pool
		   connect-timeout read-timeout max-redirects force-binary force-string want-stream
		   ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path content))
  (apply #'request uri :method :get args))

(defun post (uri &rest args
             &key version content headers basic-auth bearer-auth cookie-jar keep-alive
	       use-connection-pool connect-timeout read-timeout
               force-binary force-string want-stream
               ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version content headers basic-auth bearer-auth cookie-jar keep-alive
		   use-connection-pool connect-timeout read-timeout force-binary force-string
		   want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy
		   insecure ca-path))
  (apply #'request uri :method :post args))

(defun head (uri &rest args
             &key version headers basic-auth bearer-auth cookie-jar connect-timeout read-timeout max-redirects
               ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version headers basic-auth bearer-auth cookie-jar connect-timeout read-timeout
		   max-redirects ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (apply #'request uri :method :head :use-connection-pool nil args))

(defun put (uri &rest args
            &key version content headers basic-auth bearer-auth cookie-jar keep-alive
	      use-connection-pool connect-timeout read-timeout
              force-binary force-string want-stream
              ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version content headers basic-auth bearer-auth cookie-jar keep-alive
		   use-connection-pool connect-timeout read-timeout force-binary force-string
		   want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose
		   proxy insecure ca-path))
  (apply #'request uri :method :put args))

(defun patch (uri &rest args
              &key version content headers basic-auth bearer-auth cookie-jar keep-alive
		use-connection-pool connect-timeout read-timeout
                force-binary force-string want-stream
                ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version content headers basic-auth bearer-auth cookie-jar keep-alive
		   use-connection-pool connect-timeout read-timeout force-binary force-string
		   want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy
		   insecure ca-path))
  (apply #'request uri :method :patch args))

(defun delete (uri &rest args
               &key version headers basic-auth bearer-auth cookie-jar keep-alive
		 use-connection-pool connect-timeout read-timeout
                 force-binary force-string want-stream content
                 ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version headers basic-auth bearer-auth cookie-jar keep-alive use-connection-pool
		   connect-timeout read-timeout force-binary force-string want-stream ssl-key-file
		   ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path content))
  (apply #'request uri :method :delete args))

(defun fetch (uri destination &rest args
                              &key (if-exists :error)
                                   version headers basic-auth bearer-auth cookie-jar keep-alive use-connection-pool
		                   connect-timeout read-timeout max-redirects
                                   ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version headers basic-auth bearer-auth cookie-jar keep-alive use-connection-pool
		   connect-timeout read-timeout max-redirects ssl-key-file ssl-cert-file
		   ssl-key-password stream verbose proxy insecure ca-path))
  (unless (and (eql if-exists nil)
               (probe-file destination))
    (with-open-file (out destination
                         :direction :output
                         :if-exists if-exists
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      
      (let ((body (apply #'req:get uri :want-stream t :force-binary t
                         (std:removef args :if-exists))))
        (alexandria:copy-stream body out)
        ;; Nominally the body gets closed, but if keep-alive is nil we need to explicitly do it.
        (when (open-stream-p body)
          (close body))))))

(defun ignore-and-continue (e)
  (let ((restart (find-restart 'ignore-and-continue e)))
    (when restart
      (invoke-restart restart))))

(defun retry-request (times &key (interval 3))
  (declare (type (or function integer) interval))
  (etypecase times
    (condition
     (let ((restart (find-restart 'retry-request times)))
       (when restart
         (invoke-restart restart))))
    (integer
     (retry-request-ntimes times :interval interval))))

(defun retry-request-ntimes (n &key (interval 3))
  (declare (type integer n)
           (type (or function integer) interval))
  (let ((retries 0))
    (declare (type integer retries))
    (lambda (e)
      (declare (type condition e))
      (let ((restart (find-restart 'retry-request e)))
        (when restart
          (when (< retries n)
            (incf retries)
            (etypecase interval
              (function (funcall interval retries))
              (integer (sleep interval)))
            (invoke-restart restart)))))))
