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

(define-alien-type key-serial-t int)

(define-alien-routine add-key key-serial-t (type c-string)
  (description c-string) (payload c-string) (plen size-t) (ringid key-serial-t))

(define-alien-routine request-key key-serial-t
  (type c-string) (description c-string) (callout-info (c-string :not-null nil)) (destringid key-serial-t))

#|
#-keyutils (and (load-shared-object "/usr/lib/libkeyutils.so" :dont-save t) (push :keyutils *features*))
(let* ((kring key-spec-user-keyring) ; = -4
       (k1 (add-key "user" "test" "test" 5 kring))
       (k2 (request-key "user" "test" nil kring)))
  (print (cons k1 k2))
  (= k1 k2)) ; => T

(defvar *test-key-id*
  (let ((payload "password"))
    (alien-funcall
     (extern-alien "add_key" (function key-serial-t c-string c-string c-string size-t key-serial-t))
     "user" "test" payload (length payload) key-spec-thread-keyring)))
|#

;; may want a syscall interface too..

;; (sb-unix::strerror
;;  (alien-funcall
;;   (extern-alien "request_key" (function key-serial-t c-string c-string c-string key-serial-t))
;;   "user" "test" "test" *test-key-id*))

;; 
;; (apropos "syscall")
