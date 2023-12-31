;;; pkg.lisp --- low-level bindings to ALPM

;;; Commentary:

;;; Code:
(defpackage :alpm
  (:use :cl :std :sb-alien)
  (:export 
   :alpm-version))

(in-package :alpm)

(define-alien-loader alpm t)

(define-opaque alpm-handle)
(define-opaque alpm-db)
(define-opaque alpm-pkg)
(define-opaque alpm-trans)
(define-opaque alpm-time)

(define-alien-routine alpm-version c-string)
