;;; pkg.lisp --- CUDA packages

;; 

;;; Code:
(defpackage :cuda
  (:use :cl :std :log :sb-alien)
  (:export :load-cuda))
(in-package :cuda)
(define-alien-loader cudart "/opt/cuda/lib/" "libcudart.so.12")
