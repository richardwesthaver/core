;;; pkg.lisp --- low-level bindings to libssh2

;;; Commentary:

;;; Code:
(defpackage :ssh2
  (:use :cl :std :sb-alien)
  (:export 
   :libssh2-init :libssh2-exit :libssh2-free))

(in-package :ssh2)

(define-alien-loader ssh2 t)

(define-alien-routine libssh2-init int (flags int))

(define-alien-routine libssh2-exit void)

;; TODO
(define-alien-routine libssh2-free void (session (* )) (ptr (* void)))
