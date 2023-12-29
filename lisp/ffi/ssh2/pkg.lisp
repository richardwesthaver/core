;;; pkg.lisp --- low-level bindings to libssh2

;;; Commentary:

;;; Code:
(defpackage :ssh2
  (:use :cl :std :sb-alien)
  (:export 
   :load-ssh2))

(in-package :ssh2)

(defun load-ssh2 (&optional save) 
  (sb-alien:load-shared-object "libssh2.so" :dont-save (not save))
  (pushnew :ssh2 *features*))

(define-alien-routine libssh2-init int (flags int))

(define-alien-routine libssh2-exit void)

;; TODO
(define-alien-routine libssh2-free void (session (* )) (ptr (* void)))
