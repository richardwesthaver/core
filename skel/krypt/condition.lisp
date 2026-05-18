;;; krypt/condition.lisp --- Krypt Conditions

;;

;;; Code:
(in-package :skel/krypt)

(defcondition krypt-condition ()
  ()
  (:handler t)
  (:documentation "Condition signaled in the KRYPT system.")
  (:error-class krypt-error (error) ())
  (:warning-class krypt-warning (warning) ()))
