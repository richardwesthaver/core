;;; pkg.lisp --- low-level bindings to libkeyutils

;; key utility library interface

;; ref: https://man7.org/linux/man-pages/man7/keyutils.7.html

;;; Commentary:

;; 

;;; Code:
(defpackage :keyutils
  (:use :cl :std :sb-alien :sys)
  (:export 
   :load-keyutils
   :keyutils-version-string
   :keyutils-build-string
   :key-spec
   :key-spec*
   :add-key
   :request-key
   :keyctl
   :keyctl-get-keyring-id
   :keyctl-join-session-keyring
   :keyctl-update
   :keyctl-revoke
   :keyctl-chown
   :keyctl-setperm
   :keyctl-describe
   :keyctl-clear
   :keyctl-link
   :keyctl-unlink
   :keyctl-search
   :keyctl-read
   :keyctl-instantiate
   :keyctl-negate
   :keyctl-set-reqkey-keyring
   :keyctl-set-timeout
   :keyctl-assume-authority
   :keyctl-get-security
   :keyctl-session-to-parent
   :keyctl-reject
   :keyctl-instatiate-iov
   :keyctl-invalidate
   :keyctl-get-persistent
   :keyctl-dh-compute
   :keyctl-dh-compute-kdf
   :keyctl-pkey-query
   :keyctl-pkey-encrypt
   :keyctl-pkey-decrypt
   :keyctl-pkey-sign
   :keyctl-pkey-verify
   :keyctl-move
   :keyctl-capabilities
   :keyctl-watch-key
   :keyctl-describe-alloc
   :keyctl-read-alloc
   :keyctl-get-security-alloc
   :keyctl-dh-compute-alloc
   :find-key-by-type-and-desc))

(in-package :keyutils)
(define-alien-loader keyutils "/usr/lib/")
