;;; leb128.lisp --- Little-Endian Base 128 Variable Encoding

;; (U)LEB128 encoders based on CL-LEB128

;; see https://github.com/mahirvaluj/cl-leb128/blob/main/leb128.lisp

;;; Commentary:

;; ref: https://en.wikipedia.org/wiki/LEB128
;; opt: https://arxiv.org/abs/1503.07387 VByte
;; opt: https://arxiv.org/pdf/1709.08990 VByte streaming

;;; Code:
(in-package :std/num)

(defun encode-leb128 (i)
  "Encode an integer of arbitrary length into a leb128 unsigned-8 buffer"
  (let ((more t) (curr) (in 0) (int (make-array
                                     4
                                     :fill-pointer 0
                                     :element-type '(unsigned-byte 8)))) ;(neg (< i 0))
    (declare (fixnum i in))
    (loop while more do
      (setf curr (logand i #x7f))
      (setf i (ash i -7))
      (if (or (and (= i 0)  (= (logand curr #x40) 0))
              (and (= i -1) (= (logand curr #x40) 64)))
          (setf more nil)
          (setf curr (logior curr #x80)))
      (vector-push-extend curr int)
      (incf in))
    (let ((ret (make-array (length int) :element-type '(unsigned-byte 8) :initial-contents int)))
      ret)))

(defun read-leb128 (s &optional (start 0))
  "decode signed integer from stream. Returns (values decoded-integer
num-bytes-consumed)"
  (declare (fixnum start))
  (when (not (= start 0))
    (loop for i from 0 upto start do (read-byte s)))
  (let ((result 0) (shift 0) (curr) (counter 0))
    (declare (fixnum result shift counter))
    (loop do 
         (setf curr (read-byte s))
         (setf result (logior result (the fixnum (ash (logand curr #x7f) shift))))
         (setf shift (+ 7 shift))
         (incf counter)
         (when (= 0 (logand curr #x80))
           (if (= 64 (logand curr #x40))
               (return-from read-leb128 (values (logior result (the fixnum (ash (lognot 0) shift))) counter))
               (return-from read-leb128 (values result counter)))))))

(defun decode-leb128 (buf &optional (start 0))
  "decode signed integer from buffer. Returns (values decoded-integer
num-bytes-consumed)"
  (declare (fixnum start) (vector buf))
  (let ((result 0) (shift 0) (curr 0) (counter 0))
    (declare (fixnum result shift counter))
    (loop do 
         (setf curr (the (unsigned-byte 8) (aref buf start)))
         (setf start (+ 1 start))
         (setf result (logior result (the fixnum (ash (logand curr #x7f) shift))))
         (setf shift (+ 7 shift))
         (incf counter)
         (when (= 0 (logand curr #x80))
           (if (= 64 (logand curr #x40))
               (return-from decode-leb128 (values (logior result (the fixnum (ash (lognot 0) shift))) counter))
               (return-from decode-leb128 (values result counter)))))))

(declaim (ftype (function (integer &optional (unsigned-byte 8)) (array (unsigned-byte 8))) encode-uleb128))
(defun encode-uleb128 (int &optional size)
  "Encode an integer INT as a ULEB128 byte array with SIZE (in bytes)."
  (declare (integer int))
  (let ((more t) (curr) (in 0) (ret (make-array
                                     (if size
                                         size
                                         (if (zerop int)
                                             1
                                             (ceiling  (/ (log (+ int 1) 2) 7))))
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

(declaim (ftype (function ((vector unsigned-byte) &optional t) integer) decode-uleb128))
(defun decode-uleb128 (bits &optional (start 0))
  "Decode an unsigned integer from ULEB128 byte array."
  (let ((result 0) (shift 0) (curr) (counter 0))
    (declare (fixnum shift counter))
    (loop do 
         (setf curr (aref bits start))
         (setf start (+ 1 start))
         (setf result (logior result (ash (logand curr #x7f) shift)))
         (setf shift (+ 7 shift))
         (incf counter)
         (when (= 0 (logand curr #x80))
           (return-from decode-uleb128 (values result counter))))))

(defun read-uleb128 (s &optional (start 0))
  "Decode an arbitrarily large unsigned integer from stream. Skip
START number bytes. Return (values integer-decoded
num-bytes-consumed)"
  (declare (fixnum start))
  (when (not (= start 0))
    (loop for i from 0 upto start do (read-byte s)))
  (let ((result 0) (shift 0) (curr) (counter 0))
    (declare (fixnum shift counter))
    (loop do 
         (setf curr (read-byte s))
         (setf result (logior result (ash (logand curr #x7f) shift)))
         (setf shift (+ 7 shift))
         (incf counter)
         (when (= 0 (logand curr #x80))
           (return-from read-uleb128 (values result counter))))))
