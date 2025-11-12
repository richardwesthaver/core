;;; krypt/pkg.lisp --- Krypt Packages

;;

;;; Commentary:

;; TODO: https://qemu-project.gitlab.io/qemu/system/secrets.html#passing-secrets-via-the-linux-keyring

;; The goal of this system is to provide a secrets management tool accessible
;; to users and administrators. The MVP is a simple user-space CLI utility
;; built on top of the CRY cryptography package and leveraging keyutils for
;; session-based visibility.

;; Additionally we plan on building various forms of encrypted block storage
;; based on BlobDB + LUKS. 

;; Ultimately unsure how such an application should be delivered - probably
;; should be a separate core image for server and ship client as plugin.

;;; Code:

(defpackage :skel/krypt
  (:use :cl :std :cry/crc64 :id :ast :config :secret :cli :clap :cry/ssh :cry/gpg)
  (:export :krypt-error :*user-kryptrc* :krypt-config 
   :load-kryptrc :*krypt-directory*
   :krypt-error :simple-krypt-error
   :init-krypt :krypt-condition
   :*krypt-user-config* :*krypt-cli*
   :b3-cmd))
