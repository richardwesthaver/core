;;; io/stream.lisp --- IO Streams

;; Core IO Streams - extends SB-GRAY and STD/STREAM.

;;; Code:
(in-package :io/stream)

(defclass io-stream (stream) ())

(defgeneric ensure-file-position (self))

;;; Flex Streams
;; based on FLEXI-STREAMS
(defclass flex-stream (io-stream wrapped-stream)
  ((external-format 
    :initform (std:get-external-format :iso-8859-1)
    :initarg :external-format
    :accessor external-format
    :documentation "The encoding currently used by this stream. Can be changed on the fly.")
   (element-type 
    :initform 'character
    :initarg :element-type
    :accessor element-type
    :documentation "The element type of this stream."))
  (:documentation "A FLEX-STREAM object is a stream that's
layered atop an existing binary/bivalent stream in order to allow for
multi-octet external formats. FLEX-STREAM itself is a mixin and should not be
instantiated."))

(defmethod stream-element-type ((stream flex-stream))
  "Returns the element type that was provided by the creator of
the stream."
  (std:with-optimization (:speed 3 :safety 0)
    (element-type stream)))

(defclass flex-output-stream (flex-stream 
                               std:wrapped-character-output-stream
                               fundamental-binary-output-stream)
  ()
  (:documentation "A FLEX-OUTPUT-STREAM is a FLEX-STREAM that
can actually be instatiated and used for output."))

(defclass flex-input-stream (flex-stream 
                             fundamental-binary-input-stream
                             fundamental-character-input-stream)
  ((last-char-code 
    :initform nil
    :accessor last-char-code
    :documentation "This slot either holds NIL or the last character read successfully. This is
mainly used for UNREAD-CHAR sanity checks.")
   (last-octet 
    :initform nil
    :accessor last-octet
    :documentation "This slot either holds NIL or the last
octet read successfully from the stream using a binary operation
such as READ-BYTE. This is mainly used for UNREAD-BYTE sanity
checks.")
   (octet-stack 
    :initform nil
    :accessor octet-stack
    :documentation "A small buffer which holds octets
that were already read from the underlying stream but not yet
used to produce characters. This is mainly used if we have to
look ahead for a CR/LF line ending.")
   (position :initform 0
             :initarg :position
             :type integer
             :accessor stream-position
             :documentation "The position within the stream where each
octet read counts as one.")
   (bound :initform nil
          :initarg :bound
          :type (or null integer)
          :accessor stream-bound
          :documentation "When this is not NIL, it must be an integer
and the stream will behave as if no more data is available as soon as POSITION
is greater or equal than this value."))
  (:documentation "A FLEX-INPUT-STREAM is a FLEX-STREAM that
can actually be instatiated and used for input."))

(defclass flex-io-stream (flex-input-stream flex-output-stream)
  ()
  (:documentation "A FLEX-IO-STREAM is a FLEX-STREAM that can
actually be instatiated and used for input and output."))

(defun make-flex-stream (stream &rest args
                                 &key (external-format (std:get-external-format :iso-8859-1))
                                      element-type column position bound)
  "Create and return a new FLEX-STREAM. STREAM must be an open
binary or bivalent stream, i.e. it must be capable of reading/writing octets
with READ-SEQUENCE and/or WRITE-SEQUENCE. The resulting flex stream is an
input stream if and only if STREAM is an input stream. Likewise, it's an
output stream if and only if STREAM is an output stream. The default for
ELEMENT-TYPE is LW:SIMPLE-CHAR on LispWorks and CHARACTER on other Lisps.
EXTERNAL-FORMAT must be an EXTERNAL-FORMAT object or a symbol or a list
denoting such an object. COLUMN is the initial column of the stream which is
either a non-negative integer or NIL. The COLUMN argument must only be used
for output streams. POSITION (only used for input streams) should be an
integer and it denotes the position the stream is in - it will be increased by
one for each octet read. BOUND (only used for input streams) should be NIL or
an integer. If BOUND is not NIL and POSITION has gone beyond BOUND, then the
stream will behave as if no more input is available."
  (declare (optimize (speed 3) (safety 0)))
  ;; these arguments are ignored - they are only there to provide a
  ;; meaningful parameter list for IDEs
  (declare (ignore element-type column position bound))
  (unless (and (streamp stream)
               (open-stream-p stream))
    (error "~S should have been an open stream." stream))
  (apply #'make-instance
         ;; actual type depends on STREAM
         (cond ((and (input-stream-p stream)
                     (output-stream-p stream))
                'flex-io-stream)
               ((input-stream-p stream)
                'flex-input-stream)
               ((output-stream-p stream)
                'flex-output-stream)
               (t
                (error "~S is neither an input nor an output stream." stream)))
         :stream stream
         :external-format external-format
         (std:remove-from-plist args :external-format)))

;;; Decoding Stream
(declaim (type fixnum +buffer-size+))
(eval-always (defconstant +buffer-size+ 128))

(defclass decoding-stream (fundamental-character-input-stream)
  ((stream :type decoding-stream
           :initarg :stream
           :initform (error ":stream is required")
           :accessor decoding-stream-of)
   (external-format :initarg :external-format
             :initform (error ":external-format is required")
             :accessor decoding-stream-external-format)
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
  (with-slots (external-format) stream
    (when external-format
      ;; REVIEW 2025-06-12: was babel fn
      (setf external-format (sb-int:get-external-format (sb-int:keywordicate external-format))))))

(defun make-decoding-stream (stream &key (external-format :utf-8)
                                         (on-close))
  (let ((decoding-stream (make-instance 'decoding-stream
                           :stream stream
                           :external-format external-format
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
  (with-slots (buffer-position external-format) stream
    (< (- +buffer-size+ (the fixnum buffer-position))
       ;; REVIEW 2025-06-15: used to be babel explicit test for max char width
       (the fixnum (if (sb-impl::variable-width-external-format-p external-format) 4 2)))))

(defmethod stream-read-char ((stream decoding-stream))
  (declare (optimize speed))
  (when (needs-to-fill-buffer-p stream)
    (dec-fill-buffer stream))
  (when (= (the fixnum (decoding-stream-buffer-end-position stream))
           (the fixnum (decoding-stream-buffer-position stream)))
    (return-from stream-read-char :eof))
  (with-slots (buffer buffer-position external-format last-char last-char-size)
      stream
    (declare (fixnum buffer-position))
    ;; (let* ((mapping (print (babel-encodings:lookup-mapping babel::*string-vector-mappings* encoding)))
    ;; (counter (print (babel-encodings:code-point-counter mapping))))
    ;; (declare (type function counter))
    ;; REVIEW 2025-06-15: TEST THIS HEAVILY - removing a large code path here
    (let* ((c (schar (funcall 
                     (sb-impl::ef-octets-to-string-fun external-format)
                     buffer buffer-position 
                     (1+ buffer-position) 
                     nil)
                    0))
          (size (funcall (the function (sb-impl::ef-bytes-for-char-fun external-format)) c)))
      (declare (fixnum size))
      ;; (multiple-value-bind (chars new-end)
      ;;     (funcall counter buffer buffer-position +buffer-size+ 1)
      ;;   (declare (ignore chars) (fixnum new-end))
      ;;   (let ((string (make-string 1 :element-type 'character))
      ;;         (size (the fixnum (- new-end buffer-position))))
      ;;     (funcall (the function (print (babel-encodings:decoder mapping)))
      ;;              buffer buffer-position new-end string 0)
      (setf buffer-position (+ buffer-position size)
            last-char c
            last-char-size size)
      c)))

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

;;; Bound Stream
(defclass bound-input-stream (wrapped-stream fundamental-binary-input-stream)
  ((%position :accessor %position :initarg :position)
   (%remaining :accessor %remaining :initarg :remaining))
  (:default-initargs :remaining 0))

(defun make-bound-stream (stream size &optional start)
  (make-instance 'bound-input-stream :stream stream :remaining size
                 :position (or start (file-position stream))))

(defmethod ensure-file-position ((stream bound-input-stream))
  (let ((new-position (file-position (stream-of stream) (%position stream))))
    (unless new-position
      (error "Unable to set FILE-POSITION."))))

(defmethod stream-read-byte ((stream bound-input-stream))
  (if (zerop (%remaining stream))
      :eof
      (progn
        (ensure-file-position stream)
        (prog1 (read-byte (stream-of stream))
          (incf (%position stream))
          (decf (%remaining stream))))))

(defmethod stream-read-sequence ((stream bound-input-stream)
                                 sequence &optional start end)
  (if (zerop (%remaining stream))
      start
      (progn
        (ensure-file-position stream)
        (let ((new-end (read-sequence sequence (stream-of stream)
                                      start (min (or end (length sequence))
                                                 (+ start
                                                    (%remaining stream))))))
          (incf (%position stream) (- new-end start))
          (decf (%remaining stream) (- new-end start))
          new-end))))

;;; Peeking Stream
;; TODO 2024-11-08: make this concatenated-stream
(defclass peeking-input-stream (wrapped-stream fundamental-binary-input-stream)
  ((start
    :reader start)
   (count
    :initarg :count
    :reader peeked-count)
   (peeked
    :reader peeked)
   (unread
    :initarg :count
    :accessor unread-peeked))
  (:documentation
   "A stream that makes the first N elements available both via normal read
functions and via PEEKED."))

(defgeneric (setf peeked) (new self))

(defmethod initialize-instance :after ((self peeking-input-stream)
                                       &key stream (count 4)
                                            (element-type 'octet)
                                            (start (ignore-errors (file-position stream))))
  (unless stream
    (std:required-argument :stream))
  (setf (slot-value self 'start) start)
  (let ((buffer (make-array count :element-type element-type)))
    (read-sequence buffer stream)
    (setf (slot-value self 'peeked) buffer)))

(defmethod stream-element-type ((self peeking-input-stream))
  (if (zerop (peeked-count self))
      (stream-element-type (stream-of self))
      (array-element-type (peeked self))))

(defmethod stream-file-position ((stream peeking-input-stream) &optional spec)
  (when (start stream)
    (if spec
        (if (zerop (unread-peeked stream))
            (file-position (stream-of stream) spec)
            nil)
        (if (zerop (unread-peeked stream))
            (file-position (stream-of stream))
            (+ (- (peeked-count stream) (unread-peeked stream))
               (start stream))))))

(defmethod stream-read-byte ((stream peeking-input-stream))
  (if (zerop (unread-peeked stream))
      (read-byte (stream-of stream))
      (prog1 (aref (peeked stream) (- (length (peeked stream))
                                            (unread-peeked stream)))
        (decf (unread-peeked stream)))))

(defmethod stream-read-char ((stream peeking-input-stream))
  (if (zerop (unread-peeked stream))
      (read-char (stream-of stream))
      (prog1 (aref (peeked stream) (- (length (peeked stream))
                                      (unread-peeked stream)))
        (decf (unread-peeked stream)))))

(defmethod stream-read-sequence ((stream peeking-input-stream)
                                 sequence &optional start end)
  (if (zerop (unread-peeked stream))
      (read-sequence sequence (stream-of stream) :start start :end end)
      (let* ((end (or end (length sequence)))
             (buffer-size (- end start))
             (num-unread-peeked-remaining (unread-peeked stream)))
        (setf (subseq sequence start end) (peeked stream))
        (decf (unread-peeked stream) buffer-size)
        (if (minusp (unread-peeked stream))
            (prog1 (read-sequence sequence (stream-of stream)
                                  :start num-unread-peeked-remaining :end end)
              (setf (unread-peeked stream) 0))
            (+ start num-unread-peeked-remaining)))))

;;; Alien Streams
(defclass alien-stream (io-stream sb-gray:fundamental-stream)
  ((sap :initform nil :initarg :sap :accessor sap))
  (:default-initargs :open-p nil)
  (:documentation
   "A stream backed by a foreign (* unsigned-char)."))
