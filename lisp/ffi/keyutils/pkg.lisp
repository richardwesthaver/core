;;; pkg.lisp --- low-level bindings to libssh2

;;; Commentary:

;;; Code:
(defpackage :ssh2
  (:use :cl :std :sb-alien)
  (:export 
   :libssh2-init :libssh2-exit :libssh2-free))

(in-package :ssh2)

(define-alien-loader "keyutils" t "/usr/lib/")
