;;; krypt/pkg.lisp --- Krypt Packages

;;

;;; Commentary:

;; TODO: https://qemu-project.gitlab.io/qemu/system/secrets.html#passing-secrets-via-the-linux-keyring

;; The goal of this system is to provide a secrets management tool accessible
;; to users and administrators. The MVP is a simple user-space CLI utility
;; built on top of the CRY cryptography package and leveraging keyutils for
;; session-based visibility.

;;; Code:
(defpackage :krypt
  (:use :cl :std :cry :dat/sxp :obj/id)
  (:export :krypt-error :*default-user-kryptrc*
           :krypt-config :load-kryptrc
           :krypt-condition
           :krypt-error :simple-krypt-error))
