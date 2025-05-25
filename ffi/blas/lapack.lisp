;;; lapack.lisp --- LAPACK

;; LAPACK fortran bindings

;;; Commentary:

;; f77blas.h is <1k lines, while lapack.h is >23k. Writing bindings by hand is
;; simply impractical here.

;; As such, we're much better off using an automated approach to generating
;; our DEFAR forms.

;; The approach we're targeting right now will be based on an external lisp
;; script. This script will use the SYN/TS package to parse the entire
;; lapack.h file into an AST, then walk that AST and collect function
;; definitions, returning a list of alien definitions.

;; THIS file provides alien type definitions for the generated bindings so
;; that we only need to walk the lapack.h AST for C functions. This may change
;; in the future.

;;; Code:
(defpackage :lapack
  (:use :cl :sb-alien :blas)
  (:import-from :blas :deflapack)
  (:export))

(in-package :lapack)
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
