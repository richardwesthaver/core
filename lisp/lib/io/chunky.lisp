;;; chunky.lisp --- Chunked Streams

;; Based on Dr. Edmund Weitz's CHUNGA package.

;;; Commentary:

;; ref: https://github.com/edicl/chunga

;;; Code:
(in-package :io/chunky)
;;; Special
(defconstant +default-chunked-output-size+ 8192)
(define-constant +crlf+ (coerce #(#\Return #\Linefeed) 'string)
  :test 'string=)
(define-constant +hex-digits+ '#.(coerce "0123456789ABCDEF" 'list)
  :test 'equalp
  :documentation "The hexadecimal digits.")

;;; Utils
(defun signal-unexpected-chars (stream last-char expected-chars)
  "Signals an error that LAST-CHAR was read although one of
EXPECTED-CHARS was expected.  \(Note that EXPECTED-CHARS, despite its
name, can also be a single character instead of a list).  Calls
*CURRENT-ERROR-FUNCTION* if it's not NIL, or uses
*CURRENT-ERROR-MESSAGE* otherwise."
  (error 'syntax-error
         :stream stream
         :format-control "~%~:[End of file~;Read character ~:*~S~], ~
but expected ~:[a member of ~S~;~S~]."
         :format-arguments (list last-char
                                 (atom expected-chars)
                                 expected-chars)))

(defun charp (char)
  "Returns true if the Lisp character CHAR is a CHAR according to RFC 2616."
  (<= 0 (char-code char) 127))

(defun controlp (char)
  "Returns true if the Lisp character CHAR is a CTL according to RFC 2616."
  (or (<= 0 (char-code char) 31)
      (= (char-code char) 127)))

(defun separatorp (char)
  "Returns true if the Lisp character CHAR is a separator
according to RFC 2616."
  (find char #.(format nil " ()<>@,;:\\\"/[]?={}~C" #\Tab)
        :test #'char=))

(defun whitespacep (char)
  "Returns true if the Lisp character CHAR is whitespace
according to RFC 2616."
  (member char '(#\Space #\Tab) :test #'char=))

(defun token-char-p (char)
  "Returns true if the Lisp character CHAR is a token constituent
according to RFC 2616."
  (and (charp char)
       (not (or (controlp char)
                (separatorp char)))))

(defun assert-char (stream expected-char)
  "Reads the next character from STREAM and checks if it is the
character EXPECTED-CHAR.  Signals an error otherwise."
  (let ((char (read-char stream)))
    (unless (char= char expected-char)
      (signal-unexpected-chars stream char expected-char))
    char))

(defun assert-crlf (stream)
  "Reads the next two characters from STREAM and checks if these
are a carriage return and a linefeed.  Signals an error
otherwise."
  (assert-char stream #\Return)
  (assert-char stream #\Linefeed))

;;; Conditions
(eval-always
  (define-condition chunky-condition (condition) ()))

(eval-always
  (deferror chunky-error (chunky-condition stream-error) ()))

(deferror simple-chunky-error (chunky-condition simple-error) () (:auto t))

(define-condition chunky-warning (chunky-condition warning) ())

(define-condition simple-chunky-warning (chunky-warning simple-warning) ())

(define-condition chunky-input-unexpected-eof (chunky-error) ())

(define-condition chunky-input-corrupted (chunky-error)
  ((last-char :initarg :last-char)
   (expected-chars :initarg :expected-chars))
  (:report (lambda (c s)
             (with-slots (last-char expected-chars) c
               (format s "Chunked stream ~S appears to be corrupted. Read char ~S, but expected ~:[a member of ~S~;~S~]"
                       (stream-error-stream c)
                       last-char (atom expected-chars) expected-chars)))))

;;; Protocol
(defgeneric input-chunking-p (self)
  (:method ((self t)) nil))
(defgeneric (setf input-chunking-p) (new self))
(defgeneric extensions (self)
  (:method ((self t)) nil))
(defgeneric trailers (self)
  (:method ((self t)) nil))
(defgeneric output-chunking-p (self)
  (:method ((self t)) nil))
(defgeneric (setf output-chunking-p) (new self))
(defgeneric write-chunk (stream sequence &key start end &allow-other-keys))
;; read-chunk?
;;; Classes
(defclass chunked-stream (wrapped-stream) ())

(defclass chunked-input-stream (wrapped-stream fundamental-binary-input-stream)
  ((input-chunking-p 
    :initform nil
    :accessor input-chunking-p
    :documentation "Whether input chunking is currently enabled.")
   (input-buffer 
    :initform nil
    :documentation "A vector containing the binary
data from the most recent chunk that was read.")
   (input-position 
    :initform 0
    :accessor input-position
    :documentation "The current position within INPUT-BUFFER.")
   (input-size
    :initform 0
    :accessor input-size
    :documentation "Only the content in INPUT-BUFFER
up to INPUT-LIMIT belongs to the current chunk."))
  (:documentation "A chunked stream is of this type if its
underlying stream is an input stream. This is a subtype of
CHUNKED-STREAM."))

(defclass simple-chunked-input-stream (chunked-input-stream)
  ((extensions 
    :initform nil
    :reader extensions
    :documentation "An alist of attribute/value
pairs corresponding to the optional `chunk extensions' which
might be encountered when reading from a chunked stream.")
   (trailers 
    :initform nil
    :reader trailers
    :documentation "An alist of attribute/value
pairs corresponding to the optional `trailer' HTTP headers which
might be encountered at the end of a chunked stream.")
   (expecting-crlf-p :initform nil
                     :accessor expecting-crlf-p
                     :documentation "Whether we expect to see
CRLF before we can read the next chunk-size header part from the
stream.  \(This will actually be the CRLF from the end of the
last chunk-data part.)")
   (signal-eof :initform nil
               :accessor signal-eof
               :documentation "Return EOF after the last chunk instead
of simply switching chunking off.")))

(defmethod stream-clear-input ((stream chunked-input-stream))
  (when (input-chunking-p stream)
    (setf (input-position stream) 0
          (input-size stream) 0)))

(defmethod input-available-p ((stream chunked-input-stream))
  (< (input-position stream)
     (input-size stream)))

(defmethod stream-listen ((stream chunked-input-stream))
  (cond ((input-chunking-p stream)
         (or (input-available-p stream)
             (fill-buffer stream)))
        ;; chunked-input-stream-eof-after-last-chunk
        (t (listen (stream-of stream)))))

(defmethod fill-buffer ((stream chunked-input-stream))
  (let ((inner-stream (stream-of stream)))
        ;; set up error function for the functions in `read.lisp'
        ;;         (*current-error-function*
        ;;           (lambda (last-char expected-chars)
        ;;              "The function which is called when an unexpected
        ;; character is seen.  Signals INPUT-CHUNKING-BODY-CORRUPTED."
        ;;              (error 'input-chunking-body-corrupted
        ;;                     :stream stream
        ;;                     :last-char last-char
        ;;                     :expected-chars expected-chars)))
    (labels (
;;              (add-extensions ()
;;                "Reads chunk extensions \(if there are any) and stores
;; them into the corresponding slot of the stream."
;;                (when-let ((extensions (read-name-value-pairs inner-stream)))
;;                  (warn 'chunky-warning
;;                        :stream stream
;;                        :format-control "Adding uninterpreted extensions to stream ~S."
;;                        :format-arguments (list stream))
;;                  (setf (slot-value stream 'extensions)
;;                        (append (extensions stream) extensions)))
;;                (assert-crlf inner-stream))
             (get-chunk-size ()
               "Reads chunk size header \(including optional
extensions) and returns the size."
                 (when (expecting-crlf-p stream)
                   (assert-crlf inner-stream))
                 (setf (expecting-crlf-p stream) t)
                 ;; read hexadecimal number
                 (let (last-char)
                   (prog1 (loop for weight = (digit-char-p (setq last-char (read-char inner-stream))
                                                           16)
                                for result = (if weight
                                               (+ weight (* 16 (or result 0)))
                                               (return (or result
                                                           (error 'chunky-input-corrupted
                                                                  :stream stream
                                                                  :last-char last-char
                                                                  :expected-chars +hex-digits+)))))
                     ;; unread first octet which wasn't a digit
                     (unread-char last-char)
                     ;; (add-extensions)
                     ))))
      (let ((chunk-size (get-chunk-size)))
        (with-slots (input-buffer input-size input-position)
            stream
          (setq input-position 0
                input-size chunk-size)
          (cond ((zerop chunk-size)
                 ;; turn chunking off
                 (setf (input-chunking-p stream) nil
                       ;; (slot-value stream 'trailers)
                       ;; (read-http-headers inner-stream)
                       input-size 0)
                 ;; (when (chunked-input-stream-eof-after-last-chunk stream)
                 ;;   (setf (chunked-input-stream-eof-after-last-chunk stream) :eof))
                 ;; return NIL
                 (return-from fill-buffer))
                ((> chunk-size (length input-buffer))
                 ;; replace buffer if it isn't big enough for the next chunk
                 (setq input-buffer (make-array chunk-size :element-type '(unsigned-byte 8)))))
          (unless (= (read-sequence input-buffer inner-stream :start 0 :end chunk-size)
                     chunk-size)
            (error 'input-chunking-unexpected-end-of-file
                   :stream stream))
          chunk-size)))))

(defmethod stream-read-byte ((stream chunked-input-stream))
  (unless (input-chunking-p stream)
    (return-from stream-read-byte
      (read-byte (stream-of stream) nil :eof)))
  (unless (input-available-p stream)
    (unless (fill-buffer stream)
      (return-from stream-read-byte :eof)))
  (with-slots (input-buffer input-position)
      stream
    (prog1 (aref input-buffer input-position)
      (incf input-position))))

(defmethod stream-read-sequence ((stream chunked-input-stream)
                                 sequence &optional start end)
  (unless (input-chunking-p stream)
    (return-from stream-read-sequence
      ;; (if (eq (chunked-input-stream-eof-after-last-chunk stream) :eof)
      ;;     0
      (read-sequence sequence (stream-of stream) :start start :end end)))
  ;; )
  (loop
   (when (>= start end)
     (return-from stream-read-sequence start))   
   (unless (input-available-p stream)
     (unless (fill-buffer stream)
       (return-from stream-read-sequence start)))
   (with-slots (input-buffer input-size input-position)
       stream
     (replace sequence input-buffer
              :start1 start :end1 end
              :start2 input-position :end2 input-size)
     (let ((length (min (- input-size input-position)
                        (- end start))))
       (incf start length)
       (incf input-position length)))))
  
(defclass chunked-output-stream (wrapped-stream fundamental-binary-output-stream) 
  ((output-chunking-p :initform nil
                      :reader output-chunking-p
                      :documentation "Whether output chunking is
currently enabled.")
   (output-buffer :initform (make-array +default-chunked-output-size+ :element-type '(unsigned-byte 8))
                  :accessor output-buffer
                  :documentation "A vector used to temporarily
store data which will output in one chunk.")
   (output-position :initform 0
                 :accessor output-position
                 :documentation "The current end of OUTPUT-BUFFER."))
  (:documentation "A chunked stream is of this type if its
underlying stream is an output stream. This is a subtype of
CHUNKED-STREAM."))

(defmethod write-chunk ((stream chunked-output-stream) seq &key (start 0) (end (length seq)))
  (let ((out (stream-of stream)))
    ;; chunk size
    (loop for c across (format nil "~X" (- end start))
          do (write-byte (char-code c) out))
    (write-sequence +crlf+ out)
    ;; data
    (write-sequence seq out :start start :end end)
    (write-sequence +crlf+ out)))

(defmethod flush-buffer ((stream chunked-output-stream))
  "Uses WRITE-CHUNK to empty the output buffer unless it is
already empty."
  (with-slots (output-buffer output-position)
      stream
    (when (plusp output-position)
      (write-chunk stream output-buffer :end output-position)
      (setq output-position 0))))

(defmethod (setf output-chunking-p) (new-value (self chunked-output-stream))
  "Switches output chunking for STREAM on or off."
  (unless (eq (not new-value) (not (output-chunking-p self)))
    (with-slots (stream output-position) self
      (cond (new-value
             ;; get rid of "old" data
             (force-output stream)
             ;; initialize output buffer as being empty
             (setq output-position 0))
            (t (flush-buffer self)
               ;; last chunk to signal end of chunking
               (write-byte #.(char-code #\0) stream)
               (write-sequence +crlf+ stream)
               (write-sequence +crlf+ stream)
               (force-output stream)))))
  (setf (slot-value self 'output-chunking-p) new-value))

(defmethod stream-clear-output ((stream chunked-output-stream))
  "We clear output by resetting the output buffer and clearing
the underlying stream."
  (when (output-chunking-p stream)
    (setf (slot-value stream 'output-position) 0))
  (clear-output (stream-of stream)))

(defmethod stream-finish-output ((stream chunked-output-stream))
  "Flush the output buffer if output chunking is on, then operate
on the underlying stream."
  (when (output-chunking-p stream)
    (flush-buffer stream))
  (finish-output (stream-of stream)))

(defmethod stream-force-output ((stream chunked-output-stream))
  "Flush the output buffer if output chunking is on, then operate
on the underlying stream."
  (when (output-chunking-p stream)
    (flush-buffer stream))
  (force-output (stream-of stream)))

(defmethod stream-write-byte ((stream chunked-output-stream) byte)
  "Writes one byte by simply adding it to the end of the output
buffer \(if output chunking is enabled).  The buffer is flushed
if necessary."
  (unless (output-chunking-p stream)
    (return-from stream-write-byte
      (write-byte byte (stream-of stream))))
  (with-slots (output-position output-buffer)
      stream
    (when (>= output-position +default-chunked-output-size+)
      (flush-buffer stream))
    (setf (aref output-buffer output-position) byte)
    (incf output-position)
    byte))

(defmethod stream-write-sequence ((stream chunked-output-stream) sequence &optional start end)
  "Outputs SEQUENCE by appending it to the output buffer if it's
small enough.  Large sequences are written directly using
WRITE-CHUNK."
  (unless (output-chunking-p stream)
    (return-from stream-write-sequence
      (write-sequence sequence (stream-of stream) :start start :end end)))
  (with-slots (output-buffer output-position)
      stream
    (let ((length (- end start)))
      (cond ((<= length (- +default-chunked-output-size+ output-position))
             (replace output-buffer sequence :start1 output-position
                      :start2 start :end2 end)
             (incf output-position length))
            (t (flush-buffer stream)
               (write-chunk stream sequence :start start :end end)))))
  sequence)

(defmethod close ((stream chunked-output-stream) &key abort)
  "When a stream is closed and ABORT isn't true we have to make
sure to send the last chunk."
  (unless abort
    (setf (output-chunking-p stream) nil))
  (call-next-method))

(defclass chunked-io-stream (simple-chunked-input-stream chunked-output-stream io-stream) ())

;;; Methods
(defmethod stream-element-type ((stream chunked-stream))
  '(unsigned-byte 8))

;;; Functions
(defun make-chunked-stream (stream)
  (make-instance
      (cond ((and (input-stream-p stream)
                  (output-stream-p stream))
             'chunked-io-stream)
            ((input-stream-p stream)
             'chunked-input-stream)
            ((output-stream-p stream)
             'chunked-output-stream)
            (t 'chunked-stream))
    :stream stream))
