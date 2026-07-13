;;; std/stream.lisp --- Standard Streams

;;

;;; Code:
(in-package :std/stream)
(declaim (optimize speed))

(defun stream-fd (stream)
  "Return the posix file descriptor associated with the lisp stream."
  (etypecase stream
    (fixnum stream)
    ;; *standard-input*, *standard-output*, *terminal-io*, etc.
    (synonym-stream (sb-sys:fd-stream-fd (symbol-value (synonym-stream-symbol stream))))
    ;; sb-sys:*stdin*, *stdout*, *tty*, etc.
    (file-stream (sb-sys:fd-stream-fd stream))
    (t (sb-sys:fd-stream-fd (symbol-value (synonym-stream-symbol *standard-input*))))))

(definline copy-stream (input output &key (element-type (stream-element-type input))
                    (buffer-size 4096)
                    (buffer (make-array buffer-size :element-type element-type))
                    (start 0) end
                    finish-output)
  "Reads data from INPUT and writes it to OUTPUT. Both INPUT and OUTPUT must
be streams, they will be passed to READ-SEQUENCE and WRITE-SEQUENCE and must have
compatible element-types."
  (check-type start non-negative-integer)
  (check-type end (or null non-negative-integer))
  (check-type buffer-size positive-integer)
  (when (and end
             (< end start))
    (error "END is smaller than START in ~S" 'copy-stream))
  (let ((output-position 0)
        (input-position 0))
    (unless (zerop start)
      ;; FIX: add platform specific optimization to skip seekable streams
      (loop while (< input-position start)
            do (let ((n (read-sequence buffer input
                                       :end (min (length buffer)
                                                 (- start input-position)))))
                 (when (zerop n)
                   (error "~@<Could not read enough bytes from the input to fulfill ~
                           the :START ~S requirement in ~S.~:@>" 'copy-stream start))
                 (incf input-position n))))
    (assert (= input-position start))
    (loop while (or (null end) (< input-position end))
          do (let ((n (read-sequence buffer input
                                     :end (when end
                                            (min (length buffer)
                                                 (- end input-position))))))
               (when (zerop n)
                 (if end
                     (error "~@<Could not read enough bytes from the input to fulfill ~
                          the :END ~S requirement in ~S.~:@>" 'copy-stream end)
                     (return)))
               (incf input-position n)
               (write-sequence buffer output :end n)
               (incf output-position n)))
    (when finish-output
      (finish-output output))
    output-position))

;;; Octet Streams
;; from ironclad

;; These could be specialized for particular implementations by hooking
;; in directly to the "native" stream methods for the implementation.
(defclass octet-stream ()
  ((buffer :accessor buffer :initarg :buffer :type simple-octet-vector)))

(defmethod stream-element-type ((stream octet-stream))
  '(unsigned-byte 8))

(defmacro define-octet-stream-read-sequence (specializer type &body body)
  `(defmethod sb-gray:stream-read-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method)))))

(defmacro define-octet-stream-write-sequence (specializer type &body body)
  `(defmethod sb-gray:stream-write-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method)))))

;;;; input streams
(defclass octet-input-stream (octet-stream fundamental-binary-input-stream)
  ((index :accessor index :initarg :index :type index)
   (end :accessor end :initarg :end :type index)))

(defmethod stream-read-byte ((stream octet-input-stream))
  (let ((buffer (buffer stream))
        (index (index stream)))
    (declare (type simple-octet-vector buffer))
    (cond
      ((>= index (end stream)) :eof)
      (t
       (setf (index stream) (1+ index))
       (aref buffer index)))))

(define-octet-stream-read-sequence octet-input-stream simple-octet-vector
  (let ((buffer (buffer stream))
        (index (index stream))
        (buffer-end (end stream)))
    (declare (type simple-octet-vector buffer))
    (let* ((remaining (- buffer-end index))
           (length (- end start))
           (amount (min remaining length)))
      (replace seq buffer :start1 start :end1 end
               :start2 index :end2 buffer-end)
      (setf (index stream) (+ index amount))
      (+ start amount))))

(defun make-octet-input-stream (buffer &optional (start 0) end)
  "As MAKE-STRING-INPUT-STREAM, only with octets instead of characters."
  (declare (type simple-octet-vector buffer)
           (type array-index start)
           (type (or array-index null) end))
  (let ((end (or end (length buffer))))
    (make-instance 'octet-input-stream
                   :buffer buffer :index start :end end)))

(defmacro with-octet-input-stream ((var buffer &optional (start 0) end) &body body)
  `(with-open-stream (,var (make-octet-input-stream ,buffer ,start ,end))
     ,@body))

;;;; output streams
(defclass octet-output-stream (octet-stream fundamental-binary-output-stream)
  ((index :accessor index :initform 0 :type index)))

(defmethod stream-write-byte ((stream octet-output-stream) integer)
  (declare (type (unsigned-byte 8) integer))
  (let* ((buffer (buffer stream))
         (length (length buffer))
         (index (index stream)))
    (declare (type simple-octet-vector buffer))
    (when (>= index (length buffer))
      (let ((new-buffer (make-array (* 2 length)
                                    :element-type '(unsigned-byte 8))))
        (declare (type simple-octet-vector new-buffer))
        (replace new-buffer buffer)
        (setf buffer new-buffer
              (buffer stream) new-buffer)))
    (setf (aref buffer index) integer
          (index stream) (1+ index))
    integer))

(define-octet-stream-write-sequence octet-output-stream simple-octet-vector
  (let* ((buffer (buffer stream))
         (length (length buffer))
         (index (index stream))
         (amount (- end start)))
    (declare (type simple-octet-vector buffer))
    (when (>= (+ index amount) length)
      (let ((new-buffer (make-array (* 2 (max amount length))
                                    :element-type '(unsigned-byte 8))))
        (declare (type simple-octet-vector new-buffer))
        (replace new-buffer buffer)
        (setf buffer new-buffer
              (buffer stream) new-buffer)))
    (replace buffer seq :start1 index :start2 start :end2 end)
    (incf (index stream) amount)
    seq))

(defmethod stream-clear-output ((stream octet-output-stream))
  (setf (index stream) 0)
  nil)

(defun get-output-stream-octets (stream)
  "As GET-OUTPUT-STREAM-STRING, only with an octet output-stream instead
of a string output-stream."
  (let ((buffer (buffer stream))
        (index (index stream)))
    (setf (index stream) 0)
    (subseq buffer 0 index)))

(defun make-octet-output-stream ()
  "As MAKE-STRING-OUTPUT-STREAM, only with octets instead of characters."
  (make-instance 'octet-output-stream
                 :buffer (make-array 128 :element-type '(unsigned-byte 8))))

(defmacro with-octet-output-stream ((var) &body body)
  `(with-open-stream (,var (make-octet-output-stream))
     ,@body
     (get-output-stream-octets ,var)))

;;; Wrapped Streams
;; from SBCL manual
(defclass wrapped-stream (fundamental-stream)
  ((stream :initform nil :initarg :stream :accessor stream-of))
  (:documentation "A stream which wraps another stream accessible via STREAM-OF."))

(defmethod open-stream-p ((stream wrapped-stream))
  (open-stream-p (stream-of stream)))

(defmethod stream-element-type ((stream wrapped-stream))
  (stream-element-type (stream-of stream)))

(defmethod close ((stream wrapped-stream) &key abort)
  (close (stream-of stream) :abort abort))

(defun wrapped-stream-p (obj) 
  "Return non-nil if OBJ is of type wrapped-stream."
  (typep obj 'wrapped-stream))

(defclass wrapped-character-input-stream (wrapped-stream fundamental-character-input-stream)
  ()
  (:documentation "A wrapped CHARACTER-INPUT-STREAM."))

(defmethod stream-read-char ((stream wrapped-character-input-stream))
  (read-char (stream-of stream) nil :eof))

(defmethod stream-unread-char ((stream wrapped-character-input-stream)
                               char)
  (unread-char char (stream-of stream)))

#| example:
(with-input-from-string (input "1 2
 3 :foo  ")
  (let ((counted-stream (make-instance 'counting-character-input-stream
                         :stream input)))
    (loop for thing = (read counted-stream) while thing
       unless (numberp thing) do
         (error "Non-number ~S (line ~D, column ~D)" thing
                (line-count-of counted-stream)
                (- (col-count-of counted-stream)
                   (length (format nil "~S" thing))))
       end
       do (print thing))))
1
2
3
Non-number :FOO (line 2, column 5)
  [Condition of type SIMPLE-ERROR]
|#
(defclass counting-character-input-stream (wrapped-character-input-stream)
  ((char-count :initform 1 :accessor char-count-of)
   (line-count :initform 1 :accessor line-count-of)
   (col-count :initform 1 :accessor col-count-of)
   (prev-col-count :initform 1 :accessor prev-col-count-of))
  (:documentation "A CHARACTER-INPUT-STREAM with automatic counters:

- CHAR-COUNT via CHAR-COUNT-OF
- LINE-COUNT via LINE-COUNT-OF
- COL-COUNT via COL-COUNT-OF
- PREV-COL-COUNT via PREV-COL-COUNT-OF"))

(defmethod stream-read-char ((stream counting-character-input-stream))
  (with-accessors ((inner-stream stream-of) (chars char-count-of)
                   (lines line-count-of) (cols col-count-of)
                   (prev prev-col-count-of)) stream
      (let ((char (call-next-method)))
        (cond ((eql char :eof)
               :eof)
              ((char= char #\Newline)
               (incf lines)
               (incf chars)
               (setf prev cols)
               (setf cols 1)
               char)
              (t
               (incf chars)
               (incf cols)
               char)))))

(defmethod stream-unread-char ((stream counting-character-input-stream)
                               char)
  (with-accessors ((inner-stream stream-of) (chars char-count-of)
                   (lines line-count-of) (cols col-count-of)
                   (prev prev-col-count-of)) stream
      (cond ((char= char #\Newline)
             (decf lines)
             (decf chars)
             (setf cols prev))
            (t
             (decf chars)
             (decf cols)
             char))
      (call-next-method)))

(defclass wrapped-character-output-stream (wrapped-stream fundamental-character-output-stream)
  ((col-index :initform 0 :accessor col-index-of))
  (:documentation "A wrapped CHARACTER-OUTPUT-STREAM with the current column index accessible via
COL-INDEX-OF."))

(defmethod stream-line-column ((stream wrapped-character-output-stream))
  (col-index-of stream))

(defmethod stream-write-char ((stream wrapped-character-output-stream)
                              char)
  (with-accessors ((inner-stream stream-of) (cols col-index-of)) stream
    (write-char char inner-stream)
    (if (char= char #\Newline)
        (setf cols 0)
        (incf cols))))

#| example:
(flet ((format-timestamp (stream)
         (apply #'format stream "[~2@*~2,' D:~1@*~2,'0D:~0@*~2,'0D] "
                (multiple-value-list (get-decoded-time)))))
  (let ((output (make-instance 'prefixed-character-output-stream
                               :stream *standard-output*
                               :prefix #'format-timestamp)))
    (loop for string in '("abc" "def" "ghi") do
         (write-line string output)
         (sleep 1))))
[00:30:05] abc
[00:30:06] def
[00:30:07] ghi
NIL
|#
(defclass prefixed-character-output-stream
    (wrapped-character-output-stream)
  ((prefix :initarg :prefix :reader prefix-of))
  (:documentation "A CHARACTER-OUTPUT-STREAM which automatically writes each line of output with
a designated prefix accessible via PREFIX-OF."))

(defgeneric write-prefix (prefix stream)
  (:method ((prefix string) stream) (write-string prefix stream))
  (:method ((prefix character) stream) (write-char prefix stream))
  (:method ((prefix function) stream) (funcall prefix stream))
  (:documentation "Write a PREFIX to STREAM."))

(defmethod stream-write-char ((stream prefixed-character-output-stream)
                              char)
  (with-accessors ((inner-stream stream-of) (cols col-index-of)
                   (prefix prefix-of)) stream
    (when (zerop cols)
      (write-prefix prefix inner-stream))
    (call-next-method)))

;;; Input Macros
(defmacro with-input-from-file ((stream-name file-name &rest args
                                             &key (direction nil direction-p)
                                             &allow-other-keys)
                                &body body)
  "Evaluate BODY with STREAM-NAME to an input stream on the file
FILE-NAME."
  (declare (ignore direction))
  (when direction-p
    (error "Can't specify :DIRECTION for WITH-INPUT-FROM-FILE."))
  `(with-open-file (,stream-name ,file-name :direction :input ,@args)
     ,@body))

(defmacro with-output-to-file ((stream-name file-name &rest args
                                            &key (direction nil direction-p)
                                            &allow-other-keys)
                               &body body)
  "Evaluate BODY with STREAM-NAME to an output stream on the file
FILE-NAME."
  (declare (ignore direction))
  (when direction-p
    (error "Can't specify :DIRECTION for WITH-OUTPUT-TO-FILE."))
  `(with-open-file (,stream-name ,file-name :direction :output ,@args)
     ,@body))

(defclass timestamped-stream (prefixed-character-output-stream) ()
  (:default-initargs 
   :prefix (lambda ()
             (multiple-value-bind (secs us)
                 (floor (get-internal-real-time) internal-time-units-per-second)
               (format nil "~6,'0D.~6,'0D: " secs us))))
  (:documentation "A stream which prints all output prefixed by the interanl-real-time timestamp."))

(defclass mumble-stream (prefixed-character-output-stream) ()
  (:default-initargs
   :prefix "; ")
  (:documentation "A stream which prints all lines with a string prefix of '; '."))

(defclass fmt-stream (wrapped-character-output-stream) 
  ((formatter :initarg :formatter :accessor stream-formatter))
  (:default-initargs :stream (make-synonym-stream '*standard-output*))
  (:documentation "A wrapped stream which prints output to STREAM using a FORMATTER."))
