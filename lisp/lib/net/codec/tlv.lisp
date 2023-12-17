;;; lisp/net/codec/tlv.lisp --- TypeLengthValue wire codec

;;

;;; Code:
(in-package :net/codec/tlv)

(defstruct (tlv (:constructor %make-tlv (type length value)))
  (type 0 :type unsigned-byte) 
  (length 0 :type fixnum) 
  (value (vector) :type (vector unsigned-byte)))

(declaim (inline make-tlv))
(defun make-tlv (type val)
  (%make-tlv type (length val) val))

