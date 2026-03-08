;;; auth.lisp --- Authentication

;; Authentication Protocols

;;; Code:
(in-package :cry/auth)

(defclass authenticator () ()
  (:documentation "Base class for authentication methods."))

(defgeneric auth (self pw)
  (:method ((self password) password)
    (string= (password-hash self) (make-password-hash password (password-salt self)))))
                                 
(defgeneric supported-authenticators (self)
  (:documentation "Return a list of supported authenticators."))

(defgeneric authenticate (self obj &key &allow-other-keys)
  (:documentation "Attempt to authenticate OBJ against SELF."))

(defgeneric authenticator-textual-p (self)
  (:documentation "Return T if data should be converted to strings, nil for octet vectors."))

(defgeneric authenticator-challenge (self challenge)
  (:documentation "Feed SELF with a challenge,
which is either a string or an octet vector in accordance with the
mechanism's textuality, or :INITIAL-RESPONSE.  The method should
return one of the following:

  :CONTINUE <response>

    Continue with the authentication conversation and send <response>
    to the server.

  :OK <response>

    After sending <response> to the server the client is finished and
    expecting an :OK response.

  :ERROR

    The challenge was invalid."))

;;; Server Protocol
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
