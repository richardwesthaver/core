;;; pkg.lisp --- low-level bindings to libkeyutils

;; key utility library interface

;; ref: https://man7.org/linux/man-pages/man7/keyutils.7.html

;;; Commentary:

;; 

;;; Code:
(defpackage :keyutils
  (:use :cl :std :sb-alien)
  (:export 
   :libssh2-init :libssh2-exit :libssh2-free))

(in-package :keyutils)

(define-alien-loader "keyutils" t "/usr/lib/")

(define-alien-type key-serial-t int)
(define-alien-type key-perm-t unsigned-int)

(define-alien-type keyctl-pkey-params-len2
  (union keyctl-pkey-params-len2
         (out-len unsigned-int)
         (in2-len unsigned-int)))

(define-alien-type keyctl-pkey-params
  (struct keyctl-pkey-params
          (key-id key-serial-t)
          (in-len unsigned-int)
          (len2 keyctl-pkey-params-len2)
          (spare (array unsigned-int 7))))

#|
(defvar *test-key-id*
  (let ((payload "password"))
    (alien-funcall
     (extern-alien "add_key" (function key-serial-t c-string c-string c-string size-t key-serial-t))
     "user" "test" payload (length payload) key-spec-thread-keyring)))
|#

;; (sb-unix::strerror
;;  (alien-funcall
;;   (extern-alien "request_key" (function key-serial-t c-string c-string c-string key-serial-t))
;;   "user" "test" "test" *test-key-id*))

;; 
;; (apropos "syscall")
