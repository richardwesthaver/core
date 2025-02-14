;;; alien.lisp --- Trivial Alien Benchmarks

;; Bench Alien Objects and FFI.

;;; Commentary:

;; - Static Vectors and Stream operations

;;; Code:
(defpackage :core/bench/alien
  (:use :cl :std :rt :io/static :io/stream)
  (:export))

(in-package :core/bench/alien)

(defun static-bytes ()
  (with-open-stream (s (make-instance 'static-stream))
    (dotimes (i 20)
      (write-byte i s))
    (print (buffer s))
    (read-byte s)))
