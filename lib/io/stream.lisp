;;; io/stream.lisp --- IO Streams

;; Core IO Streams - extends SB-GRAY and STD/STREAM.

;;; Code:
(in-package :io/stream)

(defclass io-stream (stream) ())

(defgeneric ensure-file-position (self))

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
   (bytes
    :reader peeked-bytes)
   (unread-bytes
    :initarg :count
    :accessor unread-peeked-bytes))
  (:documentation
   "A stream that makes the first N elements available both via normal read
functions and via PEEKED-BYTES."))

(defmethod initialize-instance :after ((self peeking-input-stream)
                                       &key stream count
                                         (start (ignore-errors (file-position stream))))
  (setf (slot-value self 'start) start)
  (let ((buffer (make-array count :element-type '(unsigned-byte 8))))
    (read-sequence buffer stream)
    (setf (slot-value self 'bytes) buffer)))

;; (defmethod stream-element-type ((stream peeking-input-stream))
;;   '(unsigned-byte 8))

(defmethod stream-file-position ((stream peeking-input-stream) &optional spec)
  (when (start stream)
    (if spec
        (if (zerop (unread-peeked-bytes stream))
            (file-position (stream-of stream) spec)
            nil)
        (if (zerop (unread-peeked-bytes stream))
            (file-position (stream-of stream))
            (+ (- (peeked-count stream) (unread-peeked-bytes stream))
               (start stream))))))

(defmethod stream-read-byte ((stream peeking-input-stream))
  (if (zerop (unread-peeked-bytes stream))
      (read-byte (stream-of stream))
      (prog1 (aref (peeked-bytes stream) (- (length (peeked-bytes stream))
                                            (unread-peeked-bytes stream)))
        (decf (unread-peeked-bytes stream)))))

(defmethod stream-read-sequence ((stream peeking-input-stream)
                                 sequence &optional start end)
  (if (zerop (unread-peeked-bytes stream))
      (read-sequence sequence (stream-of stream) :start start :end end)
      (let* ((end (or end (length sequence)))
             (buffer-size (- end start))
             (num-unread-peeked-bytes-remaining (unread-peeked-bytes stream)))
        (setf (subseq sequence start end) (peeked-bytes stream))
        (decf (unread-peeked-bytes stream) buffer-size)
        (if (minusp (unread-peeked-bytes stream))
            (prog1 (read-sequence sequence (stream-of stream)
                                  :start num-unread-peeked-bytes-remaining :end end)
              (setf (unread-peeked-bytes stream) 0))
            (+ start num-unread-peeked-bytes-remaining)))))

;;; Alien Streams
(defclass alien-stream (io-stream sb-gray:fundamental-stream)
  ((sap :initform nil :initarg :sap :accessor sap))
  (:default-initargs :open-p nil)
  (:documentation
   "A stream backed by a foreign (* unsigned-char)."))
