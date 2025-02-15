;;; pkg.lisp --- Apache Jpeg FFI

;; 

;;; Code:
(defpackage :jpeg
  (:use :cl :std :sb-alien)
  (:export :load-jpeg))

(in-package :jpeg)

(define-alien-loader jpeg "/usr/lib/")
