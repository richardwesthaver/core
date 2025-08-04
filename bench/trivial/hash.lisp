;;; bench/trivial/hash.lisp --- Trivial Hash Benchmarks

;; From CL-BENCH

;;; Commentary:

;; hash-table benchmarking code
;;
;; some code by Paul Foley
;; Time-stamp: <2016-05-10 13:11:57 jack>

;;; Code:
(defpackage :core/bench/hash
  (:nicknames :bench/hash)
  (:use :cl :std :id :blake3)
  (:export :bench-hash-strings :bench-hash-integers))

(in-package :core/bench/hash)

(defparameter +digit+ "0123456789ABCDEF")

(defparameter +digits-needed+
  #((10 100 1000 10000 100000 10000000 100000000 536870911)
    (16 256 4096 65536 1048576 16777216 268435456 4294967296 536870911)))

(defvar *table* nil)

(defun fixnum-to-string (n base)
  (declare (fixnum n base))
  (let* ((tsize (position-if (lambda (x) (> (the fixnum x) n))
                            (aref +digits-needed+ (ash base -4))))
         (result (make-string (1+ tsize))))
    (loop for i fixnum from tsize downto 0 with q fixnum = n and r fixnum = 0
      do (multiple-value-setq (q r) (floor q base))
         (setf (schar result i) (aref +digit+ r)))
    result))

(defun bench-hash-strings (&optional (size 300) (runs 100000))
  (declare (fixnum size runs))
   (setq *table* (make-hash-table :test #'equal :size size))
   (dotimes (i runs)
     (setf (gethash (fixnum-to-string i 16) *table*) i))
   (maphash (lambda (key value) (incf (gethash key *table*) value)) *table*))
  
(defun bench-hash-integers (&optional (size 300) (runs 100000))
  (declare (fixnum size runs))
  (setq *table* (make-hash-table :test #'eql :size size))
  (dotimes (i runs)
    (setf (gethash i *table*) (1+ i)))
  (maphash (lambda (key value) (incf (gethash key *table*) value)) *table*))
