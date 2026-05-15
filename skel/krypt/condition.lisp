;;; krypt/condition.lisp --- Krypt Conditions

;;

;;; Code:
(in-package :skel/krypt)

(define-condition krypt-condition ()
  ()
  (:documentation "Condition signaled in the KRYPT package."))

(define-condition krypt-error (error krypt-condition)
  ()
  (:documentation "Error signaled from the KRYPT package."))

(deferror simple-krypt-error (krypt-error simple-error) () (:reporter t))
