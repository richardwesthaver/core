;;; lisp/net/codec/tlv.lisp --- TypeLengthValue wire codec

;;

;;; Code:
(in-package :net/codec/tlv)

(defclass tlv ()
  ((type :initform 0 :initarg :type :type octet :accessor tlv-type)
   (length :initform 0 :initarg :length :type (unsigned-byte 16) :accessor tlv-length)
   (value :initform (make-array 0 :element-type 'octet) :initarg :value :type octet-vector :accessor tlv-value))
  (:documentation "TypeLengthValue object.

TLVs are a common packet format in communication protocols. Objects of
this type are assumed to have a 1 byte TYPE, a 2 byte LENGTH, and a
VALUE which is an OCTET-VECTOR of length LENGTH."))

(defmethod sequence:length ((self tlv)) (+ (tlv-length self) 3))

(defmethod serialize ((obj tlv) (format (eql :bytes)) &key stream)
  (declare (ignore format))
  (with-slots (type length value) obj
    (let* ((end (+ 3 length))
           (buf (make-array end :element-type 'octet)))
      (setf (aref buf 0) type)
      (setf (subseq buf 1 2) (integer-to-octets length 16))
      (unless (= 0 length)
        (setf (subseq buf 3 (+ 3 length)) value))
      (if stream
          (write buf :stream stream)
          buf))))

(defun make-tlv (type length &optional (value #.(make-array 0 :element-type 'octet)))
  (make-instance 'tlv :type type :length length :value value))

(defmethod serialize ((obj tlv) (format (eql :string)) &key stream (external-format :default))
  (declare (ignore format stream))
  (sb-ext:octets-to-string (serialize obj :bytes) :external-format external-format))

(defmethod deserialize ((from simple-array) (format (eql :tlv)) &key)
  (declare (ignore format))
  (let ((type (aref from 0))
        (length (octets-to-integer (subseq from 1 3))))
    (if (= 0 length)
        (make-tlv type length)
        (let ((value (subseq from 3 (+ 3 length))))
          (funcall #'make-tlv type length value)))))

(defmethod deserialize ((from stream) (format (eql :tlv)) &key)
  (let ((type (read-byte from))
        (l (make-array 2 :element-type 'octet :adjustable t)))
    (read-sequence l from)
    (let ((length (octets-to-integer (coerce l 'octet-vector))))
      (if (= 0 length)
          (make-tlv type length nil)
          (let ((value (make-array length :element-type 'octet)))
            (read-sequence value from)
            (make-tlv type length value))))))

(defmethod serde ((from tlv) (to simple-array))
  (with-slots (type length value) from
    (setf (aref to 0) type)
    (replace to (integer-to-octets length 16) :start1 1 :start2 2)
    (unless (= 0 length)
      (replace to value :start1 3 :end1 (+ 3 length)))
    to))

(defmethod serde ((from simple-array) (to tlv))
  (if (> 3 (length from))
      (error 'serde-error :message "array length is < 3")
      (let ((type (aref from 0))
            (length (octets-to-integer (subseq from 1 2))))
        (setf (tlv-type to) type
              (tlv-length to) length
              (tlv-value to) (subseq from 3 (+ 3 length)))
        to)))
#+nil
(describe
 (deserialize
  (serialize (make-instance 'tlv) :bytes)
  :tlv))
