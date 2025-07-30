;;; pkg.lisp --- Crypto Packages

;; 

;;; Code:

#|
(ironclad:digest-file :sha1 "/tmp/picard01t0fjkc.jpg")

(cry/b3::load-blake3)
(cry/b3:b3sum "/tmp/picard01t0fjkc.jpg" :hex nil)
|#

(defpackage :cry
  (:nicknames :cryptography)
  (:shadowing-import-from :ironclad :integer-to-octets :octets-to-integer :xor)
  (:use :cl :std :sb-thread :ironclad :obj/db :obj/id)
  (:export :crypto-error :crypto-token-expired :crypto-token-invalid
   :crypto-key :token :crypto-token
   :*default-password-db* :*default-password-hasher* :*default-password-store* :*default-password-pepper*
   :password-db
   :crypto-condition))

(defpackage :cry/hotp
  (:nicknames :hotp)
  (:use :cl :std :cry)
  (:export *digits*
           *hmac-sha-mode*
           hotp))

(defpackage :cry/totp
  (:nicknames :totp)
  (:use :cl :std :cry/hotp)
  (:export *time-zero*
           *time-step-in-seconds* 
           totp))

(defpackage :cry/crc64
  (:use :cl)
  (:export :+polynomial+ :+improved-polynomial+
           :init-crc64 :crc64-stream
           :crc64-file :crc64-sequence))

(defpackage :cry/b3
  (:nicknames :b3)
  (:use :cl :std :blake3 :sb-alien)
  (:export :b3hash :b3sum
           :b3hash-string))

(defpackage :cry/jwt
  (:use :cl :std :dat/json :dat/base64 :cry)
  (:export
   #:hs256-digest
   #:compare-hs256-digest
   #:jwt-decode))

(defpackage :cry/authinfo
  (:use :cl :std :cry)
  (:export
   #:authinfo))

(defpackage :cry/keyring
  (:use :cl :std :cry :keyutils :id :db :sb-alien)
  (:export
   :get-key
   :keyring
   :make-keyring
   :clear-keys))

(defpackage :cry/password
  (:use :cl :std :obj/secret)
  (:export :password :password-hash :password-salt :make-password-hash :auth))

(defpackage :cry/drm
  (:use :cl :std))

(defpackage :cry/gpg
  (:use :cl :std :config :ast)
  (:export :*user-gpg-directory* :user-gpg-config-file :user-gpg-agent-config-file :gpg-config :gpg-agent-config))

(defpackage :cry/ssh
  (:use :cl :std :config :ast)
  (:export :*user-ssh-directory* :user-ssh-config-file :system-ssh-config-file 
   :ssh-config :sshd-config :system-sshd-config-file))
  
(defpackage :cry/auth
  (:use :cl :std)
  (:export))

(in-package :cry)

(defclass token (id) ())

(defun random-token () 
  (let ((id (make-array 64 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (dotimes (i 64)
      (vector-push (random 128) id))
    (make-instance 'token :id id)))

(defvar *password-db* nil
  "The current password database.")
(defvar *password-hasher* nil
  "The current password hasher.")
(defvar *password-store* nil
  "The current password store.")
(defvar *password-pepper* (random-token)
  "pepper value for password hashing. Make sure you change this.")

(defun token-bytes (self)
  (declare (token self))
  (id self))

(defun token-string (self)
  (declare (token self))
  (sb-ext:octets-to-string (id self)))

(defun token-hex (self)
  (declare (token self))
  (octet-vector-to-hex-string (id self)))

(defclass crypto-token (token) ())
(defclass crypto-key (token) ())
(defclass password-db (database) ())
(defclass password-store (store) ())

;;; Proto
(defgeneric register-user (user &key store password deadline)
  (:documentation "Register USER in STORE. Returns a confirmation token."))
(defgeneric get-confirmation-token (user &key store duration)
  (:documentation "Create a new user confirmation token which must be
  validated within DURATION if non-nil."))
(defgeneric confirm-registration (user confirmation &key store)
  (:documentation "Confirm USER using token response CONFIRMATION."))
(defgeneric user-pending-p (user &key store)
  (:documentation "Return non-nil if USER isn't pending confirmation, else nil."))
(defgeneric user-known-p (user &key store)
  (:documentation "Return non-nil if USER is known in STORE."))
(defgeneric authenticate-user (user password &key store)
  (:documentation "Check whether USER successfully authenticates with PASSWORD. If user
had a reset-token pending, clear it upon success."))
(defgeneric get-reset-token (user &key store duration)
  (:documentation "Create a new reset token for USER."))
(defgeneric clear-reset-token (user &key store)
  (:documentation "Clear reset token of USER."))
(defgeneric reset-password (user reset new &key store)
  (:documentation "Reset password of USER to NEW, authenticating with token RESET."))
(defgeneric delete-user (user &key store error-p)
  (:documentation "Delete USER. Signal an error if user can't be found and ERROR-P is non-nil."))
