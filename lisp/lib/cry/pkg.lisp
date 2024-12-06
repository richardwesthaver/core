(defpackage :cry
  (:nicknames :cryptography)
  (:shadowing-import-from :ironclad :integer-to-octets :octets-to-integer :xor)
  (:use :cl :std :sb-thread :sb-concurrency #+crypto :ironclad :obj/db :obj/id)
  (:export :crypto-error :crypto-token-expired :crypto-token-invalid
   :crypto-key :token :crypto-token :password
   :*default-password-db* :*default-password-hasher* :*default-password-store* :*default-password-pepper*
   :password-db))

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
  (:use :cl :std :blake3 :sb-alien :io/static)
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
  (:export))

(defpackage :cry/password
  (:use :cl :std :obj/secret)
  (:export :password :password-hash :password-salt :make-password-hash :auth))

(in-package :cry)

(defvar *password-db* nil
  "The default password database.")
(defvar *password-hasher* nil
  "The default password hasher.")
(defvar *password-store* nil
  "The default password store.")
(defvar *password-pepper* nil
  "The default pepper value for password hashing. Make sure you change this.")

(defclass token (id) ())

(defun random-token () 
  (let ((id (make-array 64 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (dotimes (i 64)
      (vector-push (random 128) id))
    (make-instance 'token :id id)))

(defgeneric token-bytes (self)
  (:method ((self token))
    (id self)))

(defgeneric token-string (self)
  (:method ((self token))
    (sb-ext:octets-to-string (obj/id:id self))))

(defclass crypto-token (token) ())
(defclass crypto-key (id) ())
(defclass password () ())
(defclass password-db (database) ())
(defclass password-store (store) ())

;;; Proto
(defgeneric register-user (user &key store password deadline)
  (:documentation "Register user identified by TOKEN in store specified by STORE. Returns
the user object and an optionally a confirmation token."))
(defgeneric get-confirmation-token (user &key store duration)
  (:documentation "Create a new user confirmation token which must be
  validated within DURATION if non-nil. Register it for USER in STORE."))
(defgeneric confirm-registration (user confirmation &key store)
  (:documentation "Confirm USER using CONFIRMATION in STORE."))
(defgeneric user-pending-p (user &key store)
  (:documentation "Return non-nil if USER isn't pending confirmation, else nil."))
(defgeneric user-known-p (user &key store)
  (:documentation "Return non-nil if USER is known in STORE."))
(defgeneric authenticate-user (user password &key store)
  (:documentation "Check whether USER successfully authenticates with PASSWORD in STORE. If user had a reset-token pending, clear it upon success."))
(defgeneric get-reset-token (user &key store duration)
  (:documentation "Create a new reset token, register it for USER in STORE for DURATION."))
(defgeneric clear-reset-token (user &key store)
  (:documentation "Clear reset token of USER."))
(defgeneric reset-password (user reset new &key store)
  (:documentation "Reset password of USER in STORE to NEW, authenticating with RESET."))
(defgeneric delete-user (user &key store error-p)
  (:documentation "Delete user identified by USER in STORE. Signal an error if user can't be found and ERROR-P is non-nil."))
