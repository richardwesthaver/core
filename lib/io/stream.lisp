;;; io/stream.lisp --- IO Streams

;; Core IO Streams - extends SB-GRAY and STD/STREAM.

;;; Code:
(in-package :io/stream)

(defclass io-stream (stream) ())

(defgeneric ensure-file-position (self))

(defmethod check-if-open (stream)
  "Checks if STREAM is open and signals an error otherwise."
  (declare (optimize (speed 3) (safety 0)))
  (unless (open-stream-p stream)
    (error 'stream-error
           :stream stream)))

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

;;; In-memory streams
(defclass in-memory-stream ()
  ((transformer 
    :initarg :transformer
    :accessor transformer
    :documentation "A function used to transform the written/read octet to the value
stored/retrieved in/from the underlying vector."))
  (:documentation "An IN-MEMORY-STREAM is a binary stream that reads
octets from or writes octets to a sequence in RAM."))

(defclass in-memory-input-stream (in-memory-stream fundamental-binary-input-stream)
  ()
  (:documentation "An IN-MEMORY-INPUT-STREAM is a binary stream that
reads octets from a sequence in RAM."))

(defclass in-memory-output-stream (in-memory-stream fundamental-binary-output-stream)
  ()
  (:documentation "An IN-MEMORY-OUTPUT-STREAM is a binary stream that
writes octets to a sequence in RAM."))

(defclass buffered-stream (io-stream)
  ((buffer 
    :initarg :buffer
    :accessor buffer
    :documentation "The underlying buffer of this stream.")))

(defclass buffered-input-stream (buffered-stream in-memory-input-stream)
  ()
  (:documentation "A binary input stream that gets its data from an
associated octet buffer."))

(defclass buffered-output-stream (buffered-stream in-memory-output-stream)
  ()
  (:documentation "A binary output stream that writes its data to an
associated octet buffer."))

(defclass list-input-stream (buffered-input-stream)
  ()
  (:documentation "A binary input stream that gets its data from an
associated list of octets."))

(defclass vector-input-stream (buffered-input-stream)
  ((idx :initarg :idx
        :accessor idx
        :type std:array-index
        :documentation "An index into the underlying vector denoting
the current position.")
   (end :initarg :end
        :accessor stream-end
        :type std:array-index
        :documentation "An index into the underlying vector denoting
the end of the available data."))
  (:documentation "A binary input stream that gets its data from an
associated octet vector."))

(defclass vector-output-stream (buffered-output-stream)
  ()
  (:documentation "A binary output stream that writes its data to an
associated vector."))

(defmethod stream-element-type ((stream in-memory-stream))
  "The element type is always OCTET by definition."
  '(unsigned-byte 8))

(defgeneric peek-byte (stream &optional peek-type eof-err-p eof-value)
  (:documentation
   "PEEK-BYTE is like PEEK-CHAR, i.e. it returns a byte from the stream without
   actually removing it. If PEEK-TYPE is NIL the next byte is returned, if
   PEEK-TYPE is T, the next byte which is not 0 is returned, if PEEK-TYPE is an
   byte, the next byte which equals PEEK-TYPE is returned. EOF-ERROR-P and
   EOF-VALUE are interpreted as usual."))

(defmethod peek-byte ((stream vector-input-stream) &optional peek-type (eof-error-p t) eof-value)
  "Returns a byte from VECTOR-INPUT-STREAM without actually removing it."
  (declare (optimize (speed 3) (safety 0)))
  (let ((index (idx stream)))
    (loop :for byte = (read-byte stream eof-error-p :eof)
       :for new-index :from index
       :until (cond ((eq byte :eof)
                     (return eof-value))
                    ((null peek-type))
                    ((eq peek-type 't)
                     (plusp byte))
                    ((= byte peek-type)))
       :finally (setf (slot-value stream 'idx) new-index)
                (return byte))))

(defmethod peek-byte ((stream list-input-stream) &optional peek-type (eof-error-p t) eof-value)
  "Returns a byte from VECTOR-INPUT-STREAM without actually removing it."
  (declare (optimize (speed 3) (safety 0)))
  (loop
     :for list-elem = (car (buffer stream))
     :for byte = (read-byte stream eof-error-p :eof)
     :until (cond ((eq byte :eof)
                   (return eof-value))
                  ((null peek-type))
                  ((eq peek-type 't)
                   (plusp byte))
                  ((= byte peek-type)))
     :finally (push list-elem (buffer stream))
              (return byte)))

(defmethod transform-octet ((stream in-memory-stream) octet)
  "Applies the transformer of STREAM to octet and returns the result."
  (declare (optimize (speed 3) (safety 0)))
  (funcall (or (transformer stream)
               #'identity) octet))

(defmethod stream-read-byte ((stream list-input-stream))
  "Reads one byte by simply popping it off of the top of the list."
  (declare (optimize (speed 3) (safety 0)))
  (check-if-open stream)
  (with-accessors ((list buffer))
      stream
    (transform-octet stream (or (pop list) (return-from stream-read-byte :eof)))))

(defmethod stream-read-char ((stream list-input-stream))
  "Reads one char and increments INDEX pointer."
  (code-char (stream-read-byte stream)))

(defmethod stream-listen ((stream list-input-stream))
  "Checks whether list is not empty."
  (declare (optimize (speed 3) (safety 0)))
  (check-if-open stream)
  (with-accessors ((list buffer))
      stream
    list))

(defmethod stream-read-sequence ((stream list-input-stream) sequence &optional (start 0) end)
  "Repeatedly pops elements from the list until it's empty."
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((list buffer))
      stream
    (loop with transformer = (transformer stream)
          for index of-type fixnum from start below end 
          while list
          do (let ((elt (pop list)))
               (setf (elt sequence index)
                     (if transformer
                         (funcall transformer elt)
                         elt)))
          finally (return index))))

(defmethod stream-read-byte ((stream vector-input-stream))
  "Reads one byte and increments INDEX pointer unless we're beyond
END pointer."
  (declare (optimize (speed 3) (safety 0)))
  (check-if-open stream)
  (with-accessors ((index idx)
                   (end stream-end)
                   (vector buffer))
      stream
    (let ((current-index index))
      (declare (fixnum current-index))
      (cond ((< current-index (the fixnum end))
             (incf (the fixnum index))
             (transform-octet stream (aref vector current-index)))
            (t :eof)))))

(defmethod stream-read-char ((stream vector-input-stream))
  "Reads one char and increments INDEX pointer."
  (code-char (stream-read-byte stream)))

(defmethod stream-listen ((stream vector-input-stream))
  "Checking whether INDEX is beyond END."
  (declare (optimize (speed 3) (safety 0)))
  (check-if-open stream)
  (with-accessors ((index idx)
                   (end stream-end))
      stream
    (< (the fixnum index) (the fixnum end))))

(defmethod stream-read-sequence ((stream vector-input-stream) sequence &optional (start 0) (end (length sequence)))
  "Traverses both sequences in parallel until the end of one of them
is reached."
  (declare (optimize (speed 3) (safety 0)))
  (loop with vector-end of-type fixnum = (stream-end stream)
        with vector = (buffer stream)
        with transformer = (transformer stream)
        for index of-type fixnum from start below end
        for vector-index of-type fixnum = (idx stream)
        while (< vector-index vector-end)
        do (let ((elt (aref vector vector-index)))
             (setf (elt sequence index)
                   (if transformer
                       (funcall transformer elt)
                       elt)))
           (incf (the fixnum (idx stream)))
        finally (return index)))

(defmethod stream-write-byte ((stream vector-output-stream) byte)
  "Writes a byte \(octet) by extending the underlying vector."
  (declare (optimize (speed 3) (safety 0)))
  (check-if-open stream)
  (with-accessors ((vector buffer))
      stream
    (vector-push-extend (transform-octet stream byte) vector)))

(defmethod stream-write-sequence ((stream vector-output-stream) sequence &optional (start 0) end)
  "Just calls VECTOR-PUSH-EXTEND repeatedly."
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((vector buffer))
      stream
    (loop for index of-type fixnum from start below (or end (length sequence))
          do (vector-push-extend (transform-octet stream (elt sequence index)) vector))
    sequence))

(defmethod stream-file-position ((stream vector-input-stream) &optional position-spec)
  "Simply returns the index into the underlying vector."
  (declare (optimize (speed 3) (safety 0)))
  (if position-spec
      (with-accessors ((index idx) (end stream-end)) stream
        (setq index
              (case position-spec
                (:start 0)
                (:end end)
                (otherwise
                 (unless (integerp position-spec)
                   (error 'sb-int::simple-stream-error
                          :format-control "Unknown file position designator: ~S."
                          :format-arguments (list position-spec)
                          :stream stream))
                 (unless (<= 0 position-spec end)
                   (error 'sb-int:simple-stream-error
                          :format-control "File position designator ~S is out of bounds."
                          :format-arguments (list position-spec)
                          :stream stream))
                 position-spec))))
        position-spec)
      (idx stream))

(defmethod stream-file-position ((stream vector-output-stream) &optional position-spec)
  "Simply returns the fill pointer of the underlying vector."
  (declare (optimize (speed 3) (safety 0)))
  (if position-spec
      (with-accessors ((vector buffer)) stream
        (let* ((total-size (array-total-size vector))
               (new-fill-pointer
                 (case position-spec
                   (:start 0)
                   (:end
                    (warn "File position designator :END doesn't really make sense for an output stream.")
                    total-size)
                   (otherwise
                    (unless (integerp position-spec)
                      (error 'in-memory-stream-position-spec-error
                             :format-control "Unknown file position designator: ~S."
                             :format-arguments (list position-spec)
                             :stream stream
                             :position-spec position-spec))
                    (unless (<= 0 position-spec array-total-size-limit)
                      (error 'in-memory-stream-position-spec-error
                             :format-control "File position designator ~S is out of bounds."
                             :format-arguments (list position-spec)
                             :stream stream
                             :position-spec position-spec))
                    position-spec))))
          (declare (fixnum total-size new-fill-pointer))
          (when (> new-fill-pointer total-size)
            (adjust-array vector new-fill-pointer))
          (setf (fill-pointer vector) new-fill-pointer)
          position-spec))
      (fill-pointer (buffer stream))))

(defmethod make-in-memory-input-stream ((vector vector) &key (start 0)
                                                             (end (length vector))
                                                             transformer)
  "Returns a binary input stream which will supply, in order, the
octets in the subsequence of VECTOR bounded by START and END.
Each octet returned will be transformed in turn by the optional
TRANSFORMER function."
  (declare (optimize (speed 3) (safety 0)))
  (make-instance 'vector-input-stream
                 :buffer vector
                 :idx start
                 :end end
                 :transformer transformer))

(defmethod make-in-memory-input-stream ((list list) &key (start 0)
                                                         (end (length list))
                                                         transformer)
  "Returns a binary input stream which will supply, in order, the
octets in the subsequence of LIST bounded by START and END.  Each
octet returned will be transformed in turn by the optional
TRANSFORMER function."
  (declare (optimize (speed 3) (safety 0)))
  (make-instance 'list-input-stream
                 :buffer (subseq list start end)
                 :transformer transformer))

(defun make-output-vector (&key (element-type '(unsigned-byte 8)))
  "Creates and returns an array which can be used as the underlying
vector for a VECTOR-OUTPUT-STREAM."
  (declare (optimize (speed 3) (safety 0)))
  (make-array 0 :adjustable t
                :fill-pointer 0
                :element-type element-type))

(defun make-in-memory-output-stream (&key (element-type '(unsigned-byte 8)) transformer)
  "Returns a binary output stream which accepts objects of type
ELEMENT-TYPE \(a subtype of OCTET) and makes available a sequence
that contains the octes that were actually output.  The octets
stored will each be transformed by the optional TRANSFORMER
function."
  (declare (optimize (speed 3) (safety 0)))
  (make-instance 'vector-output-stream
                 :buffer (make-output-vector :element-type element-type)
                 :transformer transformer))

(defmethod get-output-stream-sequence ((stream in-memory-output-stream) &key as-list)
  "Returns a vector containing, in order, all the octets that have
been output to the IN-MEMORY stream STREAM. This operation clears any
octets on STREAM, so the vector contains only those octets which have
been output since the last call to GET-OUTPUT-STREAM-SEQUENCE or since
the creation of the stream, whichever occurred most recently.  If
AS-LIST is true the return value is coerced to a list."
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((vector buffer))
      stream
    (prog1
        (if as-list
          (coerce vector 'list)
          vector)
      (setq vector
            (make-output-vector)))))

(defmethod output-stream-sequence-length ((stream in-memory-output-stream))
  "Returns the current length of the underlying vector of the
IN-MEMORY output stream STREAM."
  (declare (optimize speed))
  (length (the vector (buffer stream))))

(defmacro with-input-from-sequence ((var sequence &key start end transformer) 
                                    &body body)
  "Creates an IN-MEMORY input stream from SEQUENCE using the
parameters START and END, binds VAR to this stream and then
executes the code in BODY.  A function TRANSFORMER may optionally
be specified to transform the returned octets.  The stream is
automatically closed on exit from WITH-INPUT-FROM-SEQUENCE, no
matter whether the exit is normal or abnormal.  The return value
of this macro is the return value of BODY."
  (std:using-gensyms (decl (sequence))
    `(let (,var ,@decl)
       (unwind-protect
           (progn
             (setq ,var (make-in-memory-input-stream ,sequence
                                                     :start (or ,start 0)
                                                     :end (or ,end (length ,sequence))
                                                     :transformer ,transformer))
             ,@body)
         (when ,var (close ,var))))))

(defmacro with-output-to-sequence ((var &key as-list (element-type ''(unsigned-byte 8)) transformer)
                                   &body body)
  "Creates an IN-MEMORY output stream, binds VAR to this stream
and then executes the code in BODY.  The stream stores data of
type ELEMENT-TYPE \(a subtype of OCTET) which is \(optionally)
transformed by the function TRANSFORMER prior to storage.  The
stream is automatically closed on exit from
WITH-OUTPUT-TO-SEQUENCE, no matter whether the exit is normal or
abnormal.  The return value of this macro is a vector \(or a list
if AS-LIST is true) containing the octets that were sent to the
stream within BODY."
  `(let (,var)
     (unwind-protect
         (progn
           (setq ,var (make-in-memory-output-stream :element-type ,element-type
                                                    :transformer ,transformer))
           ,@body
           (get-output-stream-sequence ,var :as-list ,as-list))
       (when ,var (close ,var)))))

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
       (sb-ext:without-package-locks
         (the fixnum (if (symbol-call :sb-impl "VARIABLE-WIDTH-EXTERNAL-FORMAT-P" external-format) 4 2))))))

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

(defmethod stream-read-char ((stream bound-input-stream))
  (code-char (stream-read-byte stream)))

(defmethod stream-read-sequence ((stream bound-input-stream)
                                 sequence &optional (start 0) end)
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
                                            (element-type '(unsigned-byte 8))
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
                                 sequence &optional (start 0) end)
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

;;; Buffer Streams
;; TODO 2026-07-30: 

;; low-level binary deserialization using an alien buffer. Based on the
;; SYS::IO-VECTOR-CLASS metaclass.

(defvar *buffer-streams* (make-array 0 :adjustable t :fill-pointer t)
  "Vector of buffer-streams, which you can grab / return.")

(defvar *buffer-streams-lock* (sb-thread:make-mutex :name "buffer-streams"))

;; Note that the LENGTH slot is static and refers to the length of the alien
;; buffer on initialization. This is the total available space. The SIZE slot
;; refers to the total consumed elements in the buffer.

;; This class wins in situations where memory is allocated primarily by
;; foreign libraries instead of by the Lisp process - for example by a DB get
;; function called in a loop. The BUFFER-STREAM class maintains a built-in
;; cache of pre-allocated foreign buffers for us to pull from. If allocation
;; is Lisp-driven consider STATIC-STREAM instead.

;; This class is also better-suited for extension as it is not dependent on
;; the implementation-specific representation of Lisp Arrays.
(defclass buffer-stream (buffered-stream)
  ((size :initform 0 :initarg :size :accessor size)
   (offset :initform 0 :initarg :offset :accessor offset))
  (:metaclass io-vector-class))

(defaccessor sap ((self buffer-stream)) (buffer self))

(with-memoization ()
  (memoizing
   (defun buffer-stream (length)
     (or (std/macs:if-let ((class (find length (std/meta:class-direct-subclasses (find-class 'foreign-vector)) :key #'length)))
           (class-name class)
           (let* ((cl-name (intern (format nil "<BUFFER-STREAM:~a>"  length) (find-package "IO/STREAM"))))
             (compile-and-eval
              `(progn
                 (defclass ,cl-name (buffer-stream) ()
                   (:metaclass io-vector-class))
                 (setf (slot-value (find-class ',cl-name) 'length) ',length)))
             cl-name))))))

(defun make-buffer-stream (length)
  (make-instance (buffer-stream length)))

(defun buffer-stream-length (bs)
  (slot-value (class-of bs) 'length))

(defmethod element-type ((self buffer-stream)) 'octet)

(defmethod alloc ((self buffer-stream))
  (setf (sap self) (foreign-alloc `(array unsigned-char ,(buffer-stream-length self)))))

(defmethod free ((self buffer-stream))
  (unless (or (not (sap self)) (null-pointer-p (sap self)))
    (foreign-free (sap self)))
  (setf (sap self) (null-pointer)))

(defmethod reset ((self buffer-stream) &key)
  (reset-buffer-stream self))

(defparameter *bsref-range-check* t)

;; HACK 2026-08-03: consider passing in the element-type to BSREF - would simplify pointer arithmetic in the IO.
(defun bsref (x i)
  (declare (type buffer-stream x))
  (let ((n (slot-value (the buffer-stream x) 'length)))
    (assert (< -1 i n) nil 'out-of-bounds-error :requested i :bound n)
    (sap-svref (slot-value x 'buffer) 'sb-alien:unsigned-char i)))

(define-compiler-macro bsref (&whole form x i)
  (if (listp x)
      (destructuring-case x
        ((the fv obj)
         (with-gensyms (obj-v i-v n-v)
             `(lety ((,obj-v ,obj :type ,fv)
                     (,i-v ,i :type fixnum))
                ,@(when *bsref-range-check*
                    `((let ((,n-v (slot-value ,obj-v 'length)))
                        (assert (< -1 ,i-v ,n-v) nil 'out-of-bounds-error :requested ,i-v :bound ,n-v))))
                (sap-ref (slot-value (the ,fv ,obj-v) 'sap) 'unsigned-char (the fixnum (* (the fixnum ,i-v) (the fixnum 1)))))))
        ((t) form))
      form))

(defun (setf bsref) (value x i)  
  (declare (type buffer-stream x))
  (let ((n (slot-value (the buffer-stream x) 'length)))
    (assert (< -1 i n) nil 'out-of-bounds-error :requested i :bound n)
    (setf (sap-svref (slot-value x 'sap) 'sb-alien:unsigned-char i) value)))

(define-compiler-macro (setf bsref) (&whole form value x i)
  (if (and (listp x) (listp value) 
           (eql 'the (car x)) 
           (eql 'the (car value)) 
           (subtypep #1=(second x) 'buffer-stream)
           (eql (second value) (element-type #1#)))
      (let ((fv (second x))
            (lt (second value))
            (obj (third x))
            (val (third value)))
        (let ((alien-type (element-type-to-alien (element-type fv))))
          (with-gensyms (obj-v i-v n-v)
            `(lety ((,obj-v ,obj :type ,fv)
                    (,i-v ,i :type fixnum))
               ,@(if *bsref-range-check*
                     `((let ((,n-v (slot-value ,obj-v 'length)))
                         (assert (< -1 ,i-v ,n-v) nil 'out-of-bounds-error :requested ,i-v :bound ,n-v))))
               (setf (sap-ref (slot-value (the ,fv ,obj-v) 'sap) 
                              ,alien-type (the fixnum ,i-v))
                     (the ,lt ,val))))))
      form))

(defun reset-buffer-stream (bs)
  "'Empty' the buffer-stream."
  (declare (buffer-stream bs))
  (setf (size bs) 0)
  (setf (offset bs) 0))

(defun grab-buffer-stream ()
  "Grab a buffer-stream from the *buffer-streams* resource pool."
  (or (with-mutex (*buffer-streams-lock*)
        (and (plusp (length *buffer-streams*))
             (vector-pop *buffer-streams*)))
      (make-instance (buffer-stream 10))))

(defun return-buffer-stream (bs)
  "Return a buffer-stream to the *buffer-streams* resource pool."
  (reset-buffer-stream bs)
  (with-mutex (*buffer-streams-lock*)
    (vector-push-extend bs *buffer-streams*)))

(defmacro with-buffer-streams (names &body body)
  "Grab a buffer-stream, executes forms, and returns the
stream to the pool on exit."
  `(let ,(loop for name in names collect (list name '(grab-buffer-stream)))
     (declare (type buffer-stream ,@names))
     (unwind-protect
      (progn ,@body)
       (progn
     ,@(loop for name in names 
          collect (list 'return-buffer-stream name))))))

;; HACK 2026-08-03: 
(definline copy-bufs (dst dst-offset src src-offset len)
  (memcpy (sb-sys:sap+ dst dst-offset) (sb-sys:sap+ src src-offset) len))

(defun resize-buffer-stream (bs length)
  "Resize the underlying buffer of a buffer-stream, copying the old data."
  (declare (buffer-stream bs)
           (fixnum length))
  (let ((len (buffer-stream-length bs))
        (size (size bs))
        (buf (buffer bs)))
    (declare (fixnum size len)
             (alien-octets buf))
    (when (> length len)
      (let ((newlen (max length (* len 2))))
        (declare (type fixnum newlen))
        ;; FIXME: async unwinds between alloc of newbuf and free of buf will
        ;; leave us with a memory leak of size NEWLEN.  
        (let ((new (make-instance (buffer-stream newlen))))
          (when (null-pointer-p (alloc new))
            (error "Failed to allocate buffer stream of length ~A.  allocate-foreign-object returned a null pointer" newlen))
        ;; technically we just need to copy from position to size.....
          (copy-bufs (buffer new) 0 buf 0 size)
          (free bs)
          (setf bs new)
          nil)))))

(defun resize-buffer-stream-no-copy (bs length)
  "Resize the underlying buffer of a buffer-stream."
  (declare (buffer-stream bs)
           (fixnum length))
  (let ((len (buffer-stream-length bs)))
    (when (> length len)
      (let ((newlen (max length (* len 2))))
        (declare (fixnum newlen))
        ;; FIXME: async unwinds between alloc of newbuf and free of buf
        ;; will leave us with a memory leak of size NEWLEN.
        ;; (free buf)
        (free bs)
        (setf bs (make-instance (buffer-stream newlen)))
        (alloc bs)))))
      ;; (setf buf newbuf)
      ;; (setf len newlen)

(define-io :buffer
  ((octet :alias byte)
   (:read (bs)
          "Read a byte from a buffer-stream."
          (declare (buffer-stream bs))
          (let ((position (offset bs)))
            (declare (fixnum position))
            (incf (offset bs))
            (bsref bs position)))
   (:write (b bs)
           "Write a byte to a buffer-stream."
           (declare (buffer-stream bs)
                    ((unsigned-byte 8) b))
           (with-slots (size) bs
             (declare (fixnum size))
             (let ((needed (the fixnum (+ size 1))))
               (declare (fixnum needed))
               (when (> needed (the fixnum (buffer-stream-length bs))) (resize-buffer-stream bs needed))
               (setf (bsref bs size) b)
               (setf size needed)))))
  (((signed-byte 32) :alias int32)
   (:write (i bs)
           "Write a 32-bit signed integer to a buffer-stream."
           (declare (buffer-stream bs) ((signed-byte 32) i))
           (with-slots ((buf buffer) size) bs
             (let ((needed (the fixnum (+ size 4))))
               (declare (fixnum needed))
               (when (> needed (buffer-stream-length bs))
                 (resize-buffer-stream bs needed))
               (write-alien-signed-byte-32 buf i size)
               (setf size needed)
               nil)))
   (:read (bs)
          "Read a 32-bit signed integer."
          (declare (buffer-stream bs))
          (let ((position (the fixnum (offset bs))))
            (declare (fixnum position))
            (setf (offset bs) (the fixnum (+ position 4)))
            (the (signed-byte 32) (read-alien-signed-byte-32 (buffer bs) position)))))
  (fixnum32
   (:read (bs)
          "Read a 32-bit signed integer, which is assumed to be a fixnum."
          (declare (buffer-stream bs))
          (let ((position (the fixnum (offset bs))))
            (declare (fixnum position))
            (setf (offset bs) (the fixnum (+ position 4)))
            (the fixnum (read-alien-fixnum32 (buffer bs) position))))
   (:write (i bs)
  "Write a 32-bit signed integer."
  (declare (buffer-stream bs)
           (fixnum i))
  (let ((buf (buffer bs))
        (size (size bs))
        (len (buffer-stream-length bs)))
    (declare (fixnum size len)
             ((alien (* unsigned-char)) buf))
    (let ((needed (the fixnum (+ size 4))))
      (declare (fixnum needed))
      (when (> needed len) (resize-buffer-stream bs needed))
      (write-alien-fixnum32 buf i size)
      (setf size needed)
      nil))))
  (((unsigned-byte 32) :alias uint32)
   (:read (bs)
          "Read a 32-bit unsigned integer."
          (declare (buffer-stream bs))
          (let ((position (the fixnum (offset bs))))
            (declare (fixnum position))
            (setf (offset bs) (the fixnum (+ position 4)))
            (read-alien-unsigned-byte-32 (buffer bs) position)))
   (:write (u bs)
           "Write a 32-bit unsigned integer."
           (declare (buffer-stream bs)
                    ((unsigned-byte 32) u))
           (let ((buf (buffer bs))
                 (size (size bs))
                 (len (buffer-stream-length bs)))
             (declare (fixnum size len)
                      ((alien (* unsigned-char)) buf))
             (let ((needed (the fixnum (+ size 4))))
               (declare (fixnum needed))
               (when (> needed len)
                 (resize-buffer-stream bs needed))
               (write-alien-unsigned-byte-32 buf u size)
               (setf size needed)
               nil))))
  (((signed-byte 64) :alias int64)
   (:read (bs)
          "Read a 64-bit signed integer."
          (declare (buffer-stream bs))
          (let ((position (offset bs)))
            (declare (fixnum position))
            (setf (offset bs) (the fixnum (+ position 8)))
            (the (signed-byte 64) (read-alien-signed-byte-64 (buffer bs) position))))
   (:write (i bs)
           "Write a 64-bit signed integer."
           (declare (buffer-stream bs)
                    ((signed-byte 64) i))
           (let ((buf (buffer bs))
                 (size (size bs))
                 (len (buffer-stream-length bs)))
             (declare (fixnum size len)
                      ((alien (* unsigned-char)) buf))
             (let ((needed (the fixnum (+ size 8))))
               (declare (fixnum needed))
               (when (> needed len)
                 (resize-buffer-stream bs needed))
               (write-alien-signed-byte-64 buf i size)
               (setf size needed)
               nil))))
  (fixnum64
   (:read (bs)
          (declare (buffer-stream bs))
          (let ((position (offset bs)))
            (declare (fixnum position))
            (setf (offset bs) (the fixnum (+ position 8)))
            ;; Native 64-bit fixnums (NOTE: issues with non 32/64 bit fixnums?)
            (read-alien-fixnum64 (buffer bs) position)))
   (:write (i bs)
           "Write a 64-bit signed integer."
           (declare (buffer-stream bs)
                    (fixnum i))
           (let ((buf (buffer bs))
                 (size (size bs))
                 (len (buffer-stream-length bs)))
             (declare (fixnum size len)
                      ((alien (* unsigned-char)) buf))
             (let ((needed (the fixnum (+ size 8))))
               (declare (fixnum needed))
               (when (> needed len)
                 (resize-buffer-stream bs needed))
               (write-alien-fixnum64 buf i size)
               (setf size needed)
               nil))))
  (((unsigned-byte 64) :alias uint64)
   (:read (bs)
          "Read a 64-bit unsigned integer."
          (declare (buffer-stream bs))
          (let ((position (offset bs)))
            (declare (fixnum position))
            (setf (offset bs) (the fixnum (+ position 8)))
            (the (unsigned-byte 64) (read-alien-unsigned-byte-64 (buffer bs) position))))
   (:write (u bs)
           "Write a 64-bit unsigned integer."
           (declare (buffer-stream bs)
                    ((unsigned-byte 64) u))
           (let ((buf (buffer bs))
                 (size (size bs))
                 (len (buffer-stream-length bs)))
             (declare (fixnum size len)
                      ((alien (* unsigned-char)) buf))
             (let ((needed (the fixnum (+ size 8))))
               (declare (fixnum needed))
               (when (> needed len)
                 (resize-buffer-stream bs needed))
               (write-alien-unsigned-byte-64 buf u size)
               (setf size needed)
               nil))))
  ((single-float :alias float)
   (:read (bs)
          "Read a single-float."
          (declare (buffer-stream bs))
          (let ((position (the fixnum (offset bs))))
    (declare (fixnum position))
    (setf (offset bs) (the fixnum (+ position 4)))
            (the single-float (read-alien-single-float (buffer bs) position))))
   (:write (d bs)
           "Write a single-float."
           (declare (buffer-stream bs)
                    (single-float d))
           (let ((buf (buffer bs))
                 (size (size bs))
                 (len (buffer-stream-length bs)))
             (declare (fixnum size len)
                      ((alien (* unsigned-char)) buf))
             (let ((needed (the fixnum (+ size 4))))
               (declare (fixnum needed))
               (when (> needed len)
                 (resize-buffer-stream bs needed))
               (write-alien-single-float buf d size)
               (setf size needed)
               nil))))
  ((double-float :alias double)
   (:read (bs)
          "Read a double-float."
          (declare (buffer-stream bs))
          (let ((position (offset bs)))
            (declare (fixnum position))
            (setf (offset bs) (the fixnum (+ position 8)))
            (the double-float (read-alien-double-float (buffer bs) position))))
   (:write (d bs)
           "Write a double-float."
           (declare (buffer-stream bs)
                    (double-float d))
           (let ((buf (buffer bs))
                 (size (size bs))
                 (len (buffer-stream-length bs)))
             (declare (fixnum size len)
                      ((alien (* unsigned-char)) buf))
             (let ((needed (the fixnum (+ size 8))))
               (declare (fixnum needed))
               (when (> needed len)
                 (resize-buffer-stream bs needed))
               (write-alien-double-float buf d size)
               (setf size needed)
               nil))))
  (octet-vector
   (:read (bs)
          "Read the whole buffer into a fresh octet vector."
          (declare (buffer-stream bs))
          (let* ((position (offset bs))
                 (size (size bs))
                 (vlen (- size position)))
            (declare (type fixnum size vlen position))
            (when (>= vlen 0)
              (let ((v (make-array vlen :element-type '(unsigned-byte 8))))
                  (with-vector-sap (dst-ptr v)
                    (memcpy dst-ptr (sb-sys:sap+ (buffer bs) position) vlen)
                    v)))))
   (:write (bv bs)
           "Write an octet-vector into a buffer-stream, replacing the underlying buffer."
           (declare (buffer-stream bs))
           (let* ((position (offset bs))
                  (size (size bs))
                  (vlen (length bv))
                  (writable (max vlen (- size position))))
             (declare (fixnum position size vlen writable))
             (unless (zerop writable)
               (with-vector-sap (src-ptr bv)
                 (memcpy (buffer bs) src-ptr writable)
                 bs)))))
  (oid
   (:write (i bs) (write-buffer-fixnum32 i bs))
   (:read (bs) (read-buffer-fixnum32 bs))))

(defun read-buffer-to-offset (arry offset bs)
  "Read contents of buffer-stream and write them into array at offset.
Buffer relative."
  (declare (buffer-stream bs)
           (fixnum offset))
  (let* ((position (offset bs))
         (size (size bs))
         (vlen (- size position)))
    (assert (< (+ offset size) (length arry)))
    (if (>= vlen 0)
    (dotimes (i vlen arry)
      (setf (aref arry (+ i offset))
            (read-buffer-byte bs))))))

(defun write-buffer-from-offset (arry offset length bs)
  "Write array contents into buffer stream. Buffer relative."
  (declare (fixnum offset)
           (buffer-stream bs))
  (dotimes (i length arry)
    (write-buffer-byte (aref arry (+ i offset)) bs)))
