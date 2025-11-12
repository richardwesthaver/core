;;; cry/condition.lisp --- Crypto Conditions

;; 

;;; Code:
(in-package :cry-int)

(define-condition crypto-condition () ())
(define-condition crypto-error (crypto-condition std-error) ())

(define-condition crypto-token-expired (crypto-error)
  ((token :initarg :token :accessor token)
   (expiry :initarg :expiry :accessor expiry))
  (:report
   (lambda (c s)
     (format s "Token expired: ~A at ~A"
             (token c)
             (expiry c))))
  (:documentation "Condition raised when use of an expired token is attempted."))

(define-condition crypto-token-invalid (crypto-error)
  ((token :initarg :token :accessor token)
   (reset-token :initarg :reset-token :accessor reset-token))
  (:report
   (lambda (c s)
     (format s "~A reset token is invalid: ~A"
             (token c)
             (reset-token c))))
  (:documentation "Condition raised when use of an invalid token is attempted."))
