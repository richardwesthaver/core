;;; pkg.lisp --- low-level bindings to libkeyutils

;; key utility library interface

;; ref: https://man7.org/linux/man-pages/man7/keyutils.7.html

;;; Commentary:

;; 

;;; Code:
(defpackage :keyutils
  (:use :cl :std :sb-alien)
  (:export 
   :load-keyutils
   :keyutils-version-string
   :keyutils-build-string
   :key-spec
   :key-spec*))

(in-package :keyutils)
(define-alien-loader keyutils "/usr/lib/")
