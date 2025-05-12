;;; pkg.lisp --- low-level bindings to libssh2

;;; Commentary:

;;; Code:
(defpackage :ssh2
  (:use :cl :std :sb-alien)
  (:export 
   :load-ssh2 :libssh2-init :libssh2-exit :libssh2-free))

(in-package :ssh2)

(define-alien-loader ssh2 "/usr/lib/")

(defar libssh2-init int (flags int))

(defar libssh2-exit void)

;; TODO
(defar libssh2-free void (session (* t)) (ptr (* t)))
