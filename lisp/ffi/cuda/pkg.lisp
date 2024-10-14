;;; pkg.lisp --- CUDA packages

;; 

;;; Code:
(defpackage :cuda
  (:use :cl :std :log :sb-alien)
  (:export :load-cuda))
(in-package :cuda)
;; use the stubs
(define-alien-loader cuda "/opt/cuda/lib/stubs/")
(define-alien-loader cufft "/opt/cuda/lib/stubs/")
(define-alien-loader cufftw "/opt/cuda/lib/stubs/")
(define-alien-loader cublas "/opt/cuda/lib/stubs/")
(define-alien-loader nvrtc "/opt/cuda/lib/stubs/")
(define-alien-loader nvfatbin "/opt/cuda/lib/stubs/")
(define-alien-loader cusparse "/opt/cuda/lib/stubs/")

