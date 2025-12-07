;;; keyutils.lisp --- Keyutils types

;; 

;;; Code:
(in-package :keyutils)

(define-alien-type iovec (struct nil))

(define-alien-type key-serial-t (integer 32))
(define-alien-type key-perm-t (unsigned 32))

(define-alien-type keyctl-pkey-params (struct keyctl-pkey-params
                                              (key-id key-serial-t)
                                              (len1 unsigned-int)
                                              (len2 unsigned-int)
                                              (%sparse (array unsigned-int 7))))

(define-alien-variable keyutils-version-string (array char))
(define-alien-variable keyutils-build-string (array char))
;; (cast keyutils-version-string c-string) ;= "keyutils-1.6.3"
;; (cast keyutils-build-string c-string) ;= nil

;; TODO: recursive_key_scanner_t

(define-alien-enum (key-spec)
                   :thread +key-spec-thread-keyring+
                   :process +key-spec-process-keyring+
                   :session +key-spec-session-keyring+
                   :user +key-spec-user-keyring+
                   :user-session +key-spec-user-session-keyring+
                   :group +key-spec-group-keyring+
                   :reqkey-auth +key-spec-reqkey-auth-key+)
