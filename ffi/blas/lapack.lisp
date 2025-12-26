;;; lapack.lisp --- LAPACK

;; LAPACK fortran bindings

;;; Commentary:

;; f77blas.h is <1k lines, while lapack.h is >23k. Writing bindings by hand is
;; simply impractical here.

;; As such, we're much better off using an automated approach to generating
;; our DEFAR forms.

;; The approach we're using right now is based on an external lisp
;; script. This script uses the SYN/TS (tree-sitter) package to parse the
;; entire lapack.h file into an AST, then walk that AST and collect function
;; definitions, returning a list of alien definitions (as DEFAR forms).

;; This file provides alien type definitions for the generated bindings so
;; that we only need to walk the lapack.h AST for C functions.

#|
LAPACK routines use the following matrix storage schemes:

Full storage: an m-by-n matrix A is stored in a two-dimensional array a, with
the matrix element aij (i = 1..mj = 1..n), and stored in the array element
a(i,j).

Packed storage scheme allows you to store symmetric, Hermitian, or triangular
matrices more compactly: the upper or lower triangle of the matrix is packed
by columns in a one-dimensional array.

Band storage: an m-by-n band matrix with kl sub-diagonals and ku
superdiagonals is stored compactly in a two-dimensional array ab with kl+ku+1
rows and n columns. Columns of the matrix are stored in the corresponding
columns of the array, and diagonals of the matrix are stored in rows of the
array.

Rectangular Full Packed (RFP) storage: the upper or lower triangle of the
matrix is packed combining the full and packed storage schemes. This
combination enables using half of the full storage as packed storage while
maintaining efficiency by using Level 3 BLAS/LAPACK kernels as the full
storage.

Generally in LAPACK routines, arrays that hold matrices in packed storage have
names ending in p; arrays with matrices in band storage have names ending in
b; arrays with matrices in the RFP storage have names ending in fp.
|#

;; lapack description notation ref: 
;; https://www.intel.com/content/www/us/en/docs/onemkl/developer-reference-fortran/2025-1/mathematical-notation-for-lapack-routines.html
;;; Code:
(defpackage :lapack
  (:use :std :cl :sb-alien :blas)
  (:export
   #:load-lapack-ffi
   #:compile-lapack-ffi
   #:load-lapack
   #:load-lapacke))

(in-package :lapack)
(push :lapack *features*)
(define-alien-loader lapack "/usr/lib/")
(define-alien-loader lapacke "/usr/lib/")

;; types
(define-alien-type lapack-int int)
(define-alien-type lapack-logical lapack-int)
(define-alien-type lapack-float-return float)
(define-alien-type lapack-complex-float complex-float)
(define-alien-type lapack-complex-double complex-double)
;; callbacks
(define-alien-type lapack-s-select2 (* (function lapack-logical (* float) (* float))))
(define-alien-type lapack-s-select3 (* (function lapack-logical (* float) (* float) (* float))))
(define-alien-type lapack-d-select2 (* (function lapack-logical (* double) (* double))))
(define-alien-type lapack-d-select3 (* (function lapack-logical (* double) (* double) (* double))))
(define-alien-type lapack-c-select1 (* (function lapack-logical (* lapack-complex-float))))
(define-alien-type lapack-c-select2 (* (function lapack-logical (* lapack-complex-float) (* lapack-complex-float))))
(define-alien-type lapack-z-select1 (* (function lapack-logical (* lapack-complex-double))))
(define-alien-type lapack-z-select2 (* (function lapack-logical (* lapack-complex-double) (* lapack-complex-double))))

(defparameter *lapack-ffi* (asdf:system-relative-pathname :blas "lapack-ffi.lisp"))
(defun load-lapack-ffi (&optional (file *lapack-ffi*))
  (load file))

(defun compile-lapack-ffi (&optional (file *lapack-ffi*))
  (compile-file file))

(defmacro deflapack (sym ret &rest args)
  `(defar (,(concatenate 
             'string 
             (substitute #\_ #\- 
                         (string-downcase 
                          (symbol-name sym)))
             "_")
           ,sym)
     ,ret
     ,@args))
