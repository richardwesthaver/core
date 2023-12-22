(in-package :log)

(define-condition log-error (simple-error program-error) ()
  (:documentation "Base class for all LOG errors"))
