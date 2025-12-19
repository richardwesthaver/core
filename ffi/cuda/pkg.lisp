;;; pkg.lisp --- CUDA packages

;; 

;;; Code:
(defpackage :cuda
  (:use :cl :std :log :sb-alien)
  (:export :load-cuda :load-cudnn
           :device-compute-capability))

(in-package :cuda)

(defvar *cuda-home* #P"/opt/cuda/")
(defvar *cuda-lib-path* #P"/opt/cuda/lib/")

;; use the stubs
(define-alien-loader cuda "/usr/lib/")
(define-alien-loader cudnn "/usr/lib/")
(define-alien-loader cublas *cuda-lib-path*)
(define-alien-loader cufft *cuda-lib-path*)
(define-alien-loader cufftw *cuda-lib-path*)
(define-alien-loader cudart *cuda-lib-path*)

(define-condition cuda-error (error)
  ((name :initarg :name :reader error-name)
   (code :initarg :code :reader error-code))
  (:report 
   (lambda (c s)
     (format s "A driver API error occurred while calling ~A: ~A" (error-name c) (error-code c)))))

(defun check-cuda-error (name code)
  (assert (zerop code) nil 'cuda-error :code code :name name))
