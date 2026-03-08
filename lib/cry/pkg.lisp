;;; pkg.lisp --- Crypto Packages

;; 

;;; Code:
(defpkg :cry/otp
  (:nicknames :otp)
  (:use :cl :std :cry-int)
  (:export 
   :*digits*
   :*hmac-sha-mode*
   :hotp
   :*time-zero*
   :*time-step-in-seconds* 
   :totp))

(defpkg :cry/crc64
  (:use :cl)
  (:export :+polynomial+ :+improved-polynomial+
           :init-crc64 :crc64-stream
           :crc64-file :crc64-sequence))

(defpkg :cry/jwt
  (:use :cl :std :dat/json :dat/base64 :cry-int)
  (:export
   #:hs256-digest
   #:compare-hs256-digest
   #:jwt-decode))

(defpkg :cry/authinfo
  (:use :cl :std :cry-int)
  (:export
   #:authinfo))

(defpkg :cry/keyring
  (:use :cl :std :cry-int :keyutils :id :db :sb-alien)
  (:export
   :get-key
   :keyring
   :make-keyring
   :clear-keys))

(defpkg :cry/password
  (:use :cl :std :obj/secret)
  (:export :password :password-hash :password-salt :make-password-hash))

(defpkg :cry/drm
  (:use :cl :std))

(defpkg :cry/gpg
  (:use :cl :std :config :ast)
  (:export :*user-gpg-directory* :user-gpg-config-file :user-gpg-agent-config-file :gpg-config :gpg-agent-config))

(defpkg :cry/ssh
  (:use :cl :std :config :ast)
  (:export :*user-ssh-directory* :user-ssh-config-file :system-ssh-config-file 
   :ssh-config :sshd-config :system-sshd-config-file))
  
(defpkg :cry/sign
  (:use :cl :std :config :ast :secret)
  (:export))

(defpkg :cry/auth
  (:use :cl :std :cry/password)
  (:export :authenticator :authenticator-textual-p :authenticator-challenge :auth :authenticate))
