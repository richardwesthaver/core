;;; pkg.lisp --- BLAS packages

;; 

;;; Code:
(defpackage :blas
  (:use :cl :std :log :sb-alien)
  (:export :load-openblas :load-blas :load-lapack :load-lapacke :load-cblas
   :dgemm))

(in-package :blas)
(define-alien-loader openblas "/usr/lib/")
;; usually just points to libopenblas.so
(define-alien-loader blas "/usr/lib/")
(define-alien-loader cblas "/usr/lib/")
(define-alien-loader lapack "/usr/lib/")
(define-alien-loader lapacke "/usr/lib/")
;; these are part of CBLAS
(define-alien-routine openblas-get-num-threads int)
(define-alien-routine openblas-set-num-threads-local int (n int))
(define-alien-routine openblas-get-num-procs int)
(define-alien-routine openblas-get-config c-string)
(define-alien-routine openblas-get-corename c-string)
(define-alien-routine openblas-set-threads-callback-function void (* function))
;; (define-alien-routine openblas-setaffinity int (thread-idx int) (cpusetsize size-t) (cpu-set (* cpu-set-t)))
;; (define-alien-routine openblas-getaffinity int (thread-idx int) (cpusetsize size-t) (cpu-set (* cpu-set-t)))
(define-alien-routine openblas-get-parallel int)
(define-alien-enum (openblas-parallel int)
  :sequential 0
  :thread 1
  :openmp 2)

;; exported by f77blas.h
;; this is defined literally (no BLASFUNC)
(define-alien-routine ("openblas_set_num_threads_" openblas-set-num-threads) void (n int :copy))

;; RETURN ON STACK (f77blas.h, libblas.so)
(defmacro blasfunc (sym ret &rest args)
  `(define-alien-routine (,(concatenate 'string (string-downcase (symbol-name sym)) "_") ,sym) ,ret ,@args))

;; FLOATRET = float
;; blasint = int
;; BLASLONG = long
;; BLASULONG unsigned-long

;;; Level 1
;;; Level 2
;;; Level 3
(blasfunc dgemm void
  (transa char :copy)
  (transb char :copy)
  (m int :copy)
  (n int :copy)
  (k int :copy)
  (alpha double :copy)
  (a (* double))
  (lda int :copy)
  (b (* double))
  (ldb int :copy)
  (beta double :copy)
  (c (* double))
  (ldc int :copy))
