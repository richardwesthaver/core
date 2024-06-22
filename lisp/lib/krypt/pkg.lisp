;;; krypt/pkg.lisp --- Krypt Packages

;;

;;; Commentary:

;; TODO: https://qemu-project.gitlab.io/qemu/system/secrets.html#passing-secrets-via-the-linux-keyring

;;; Code:
(defpackage :krypt
  (:use :cl :std :cry :dat/sxp :obj/id)
  (:export :krypt-error :*default-user-kryptrc*
           :krypt-config :load-kryptrc))
