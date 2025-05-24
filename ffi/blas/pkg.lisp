;;; pkg.lisp --- BLAS packages

;; 

;;; Code:
(defpackage :blas
  (:use :cl :std :log :sb-alien)
  (:export :load-openblas :load-blas :load-lapack :load-lapacke :load-cblas
   :dgemm
   :xdouble
   :complex-float
   :complex-double
   :complex-xdouble))

(in-package :blas)
(define-alien-loader openblas "/usr/lib/")
;; usually just points to libopenblas.so
(define-alien-loader blas "/usr/lib/")
(define-alien-loader cblas "/usr/lib/")
(define-alien-loader lapack "/usr/lib/")
(define-alien-loader lapacke "/usr/lib/")
;;; CBLAS
;; these are part of CBLAS
(defar openblas-get-num-threads int)
(defar openblas-set-num-threads-local int (n int))
(defar openblas-get-num-procs int)
(defar openblas-get-config c-string)
(defar openblas-get-corename c-string)
(defar openblas-set-threads-callback-function void (* (function void)))
;; (defar openblas-setaffinity int (thread-idx int) (cpusetsize size-t) (cpu-set (* cpu-set-t)))
;; (defar openblas-getaffinity int (thread-idx int) (cpusetsize size-t) (cpu-set (* cpu-set-t)))
(defar openblas-get-parallel int)
(define-alien-enum (openblas-parallel int)
  :sequential 0
  :thread 1
  :openmp 2)

;; this is defined literally (no BLASFUNC)
(define-alien-routine ("openblas_set_num_threads_" openblas-set-num-threads) void (n int :copy))

;;; Types
;; FLOATRET = float
;; blasint = int
;; BLASLONG = long
;; BLASULONG unsigned-long
;; xdouble double?
;; bfloat16 unsigned-short

(define-alien-type xdouble (array unsigned-long 2))
(define-alien-type complex-float (array float 2))
(define-alien-type complex-double (array double 2))
(define-alien-type complex-xdouble (array xdouble 2))
(define-alien-type openblas-dojob-callback 
  (function void int (* t) int))
(define-alien-type openblas-threads-callback
    (function void int (* openblas-dojob-callback) int size-t (* t) int))

;;; Shared Macros      
(defmacro defblas (sym ret &rest args)
  `(defar (,(concatenate 'string (string-downcase (symbol-name sym)) "_") ,sym) ,ret ,@args))

(defmacro deflapack (sym ret &rest args)
  `(defar (,(concatenate 'string "LAPACK_" (substitute #\_ #\- (string-downcase (symbol-name sym)))) ,sym) ,ret ,@args))
#|
 XERBLA  is an error handler for the LAPACK routines.
 It is called by an LAPACK routine if an input parameter has an
 invalid value.  A message is printed and execution stops.

 Installers may consider modifying the STOP statement in order to
 call system-specific exception-handling facilities.
|#
(defblas xerbla int
  (srname c-string)
  (info int :copy)
  (nout int))
