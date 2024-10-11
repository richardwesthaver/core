;;; pkg.lisp --- low-level bindings to librustls

;;; Commentary:

;;; Code:
(defpackage :rustls
  (:use :cl :sb-alien :std/alien)
  (:export :load-rustls))

(in-package :rustls)

(define-alien-loader rustls "/usr/lib/")
