;;; alien.lisp --- Trivial Alien Benchmarks

;; Bench Alien Objects and FFI.

;;; Commentary:

;; - Static Vectors and Stream operations

;;; Code:
(defpackage :core/bench/alien
  (:use :cl :std :rt :io/static :io/stream)
  (:export))

(in-package :core/bench/alien)

(defvar *static-io-element-type* 'fixnum)

(defun static-iota-stream (n)
  (with-static-stream (s :size n :element-type *static-io-element-type*)
    (dotimes (i n)
      (sb-gray:stream-write-byte s i))
    s))
