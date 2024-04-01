;;; lisp/net/codec/tlv.lisp --- TypeLengthValue wire codec

;;

;;; Code:
(in-package :net/codec/tlv)

(defclass tlv ()
  ((type :initform 0 :type octet)
   (length :initform 0 :type fixnum)
   (value :initform #() :type octet-vector)))

(defmethod serialize ((obj tlv) (format (eql :bytes)) &key stream)
  (declare (ignore format))
  (with-slots (type length value) obj
    (let ((buf (make-array (+ 9 length) :element-type 'octet)))
      (setf (aref buf 0) type)
      (setf (subseq buf 1 8) (integer-to-octets length))
      (setf (subseq buf 9) value)
      (if stream
          (write buf :stream stream)
          buf))))

;; (defmethod deserialize)
