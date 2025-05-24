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
(in-package :blas)

(define-alien-type lapack-int int)
(define-alien-type lapack-logical lapack-int)
(define-alien-type lapack-float-return float)
