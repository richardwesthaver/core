;;; io/stream.lisp --- IO Streams

;; Core IO Streams - extends SB-GRAY and STD/STREAM.

;;; Code:
(in-package :io/stream)

(defclass io-stream () ())

(defgeneric ensure-file-position (self))

(defclass bound-input-stream (wrapped-stream fundamental-binary-input-stream)
  ((%position :accessor %position :initarg :position)
   (%remaining :accessor %remaining :initarg :remaining))
  (:default-initargs :remaining 0))

(defun make-bound-stream (stream size &optional start-position)
  (make-instance 'bound-input-stream :stream stream :remaining size
                 :position (or start-position (file-position stream))))

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
