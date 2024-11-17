;;; simd.lisp --- Trivial SIMD Benchmarks

;; 

;;; Code:
(require 'sb-simd)
(defpackage :core/bench/simd
  (:nicknames :bench/simd)
  (:use :cl :std :id)
  (:export))
(in-package :core/bench/simd)

(defun simd= (a b)
  (typecase a
    (sb-ext:simd-pack
     (when (sb-ext:simd-pack-p b)
       (multiple-value-bind (a0 a1) (sb-ext:%simd-pack-ub64s a)
         (multiple-value-bind (b0 b1) (sb-ext:%simd-pack-ub64s b)
           (and (= a0 b0) (= a1 b1))))))
    (sb-ext:simd-pack-256
     (when (sb-ext:simd-pack-256-p b)
       (multiple-value-bind (a0 a1 a2 a3) (sb-ext:%simd-pack-256-ub64s a)
         (multiple-value-bind (b0 b1 b2 b3) (sb-ext:%simd-pack-256-ub64s b)
           (and (= a0 b0) (= a1 b1) (= a2 b2) (= a3 b3))))))
    (otherwise nil)))
