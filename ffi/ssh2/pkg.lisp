;;; pkg.lisp --- low-level bindings to libssh2

;;; Commentary:

;;; Code:
(defpackage :ssh2
  (:use :cl :std :sb-alien)
  (:export 
   :load-ssh2 :libssh2-init :libssh2-exit :libssh2-free
   :ssh-disconnect :ssh-disconnect* :libssh2-error :libssh2-error*
   :libssh2-session-init-ex
   :libssh2-session-supported-algs))

(in-package :ssh2)

(define-alien-loader ssh2 "/usr/lib/")
