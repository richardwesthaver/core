;;; pkg.lisp --- low-level bindings to libwasmer

;;; Commentary:

;;; Code:
(defpackage :wasmer
  (:use :cl :sb-alien :std/alien)
  (:export :load-wasmer))

(in-package :wasmer)

(define-alien-loader wasmer "/usr/lib/")
