;;; pkg.lisp --- libmatroska FFI

;; 

;;; Commentary:

;;; Code:
(defpackage :matroska
  (:use :cl :std :sb-alien)
  (:export :load-matroska))

(in-package :matroska)

(define-alien-loader matroska "/usr/lib/")
