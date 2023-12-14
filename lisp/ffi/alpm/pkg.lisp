;;; pkg.lisp --- low-level bindings to ALPM

;;; Commentary:

;;; Code:
(defpackage :alpm
  (:use :cl :std :sb-alien)
  (:export 
   :load-alpm
   :alpm-version))

(in-package :alpm)

(defun load-alpm () 
  (unless (member :alpm *features*)
    (sb-alien:load-shared-object "libalpm.so" :dont-save t)
    (push :alpm *features*)))
(load-alpm)

(define-opaque alpm-handle)
(define-opaque alpm-db)
(define-opaque alpm-pkg)
(define-opaque alpm-trans)
(define-opaque alpm-time)

(define-alien-routine alpm-version c-string)
