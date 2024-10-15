;;; pkg.lisp --- CUDA packages

;; 

;;; Code:
(defpackage :cuda
  (:use :cl :std :log :sb-alien)
  (:export :load-cuda :load-cudnn))

(in-package :cuda)
;; use the stubs
(define-alien-loader cuda "/usr/lib/")
(define-alien-loader cudnn "/usr/lib/")
