;;; leb128.lisp --- Little-Endian Base 128 Variable Encoding

;; (U)LEB128 encoders

;;; Commentary:

;; ref: https://en.wikipedia.org/wiki/LEB128
;; opt: https://arxiv.org/abs/1503.07387 VByte
;; opt: https://arxiv.org/pdf/1709.08990 VByte streaming

;;; Code:
(in-package :std/num)

(defun encode-unsigned-leb128 (int)
  "Encode an integer INT as an octet-vector with LEB128 encoding."
  (declare (fixnum int))
  (let ((more t) (curr) (in 0) (ret (make-array
                                     (if (zerop int)
                                         1
                                         (ceiling  (/ (log (+ int 1) 2) 7)))
                                     :element-type '(unsigned-byte 8)))) ;(neg (< int 0))
    (loop while more do
         (setf curr (logand int #x7f))
         (setf int (ash int -7))
         (if (= int 0)
             (setf more nil)
             (setf curr (logior curr #x80)))
         (setf (aref ret in) curr)
         (incf in))
    ret))

(declaim (ftype (function ((simple-array unsigned-byte) &optional t) fixnum) decode-unsigned-leb128))
(defun decode-unsigned-leb128 (bits &optional (start 0))
  "Decode an unsigned integer from LEB128-encoded octet-vector BITS."
  (declare (type (array unsigned-byte) bits))
  (let ((result 0) (shift 0) (curr) (counter 0))
    (declare (fixnum result shift counter))
    (loop do 
         (setf curr (the (unsigned-byte 8) (aref bits start)))
         (setf start (+ 1 start))
         (setf result (logior result (the fixnum (ash (logand curr #x7f) shift))))
         (setf shift (+ 7 shift))
         (incf counter)
         (when (= 0 (logand curr #x80))
           (return-from decode-unsigned-leb128 (values result counter))))))
