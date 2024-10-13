;; log/err.lisp --- errors which may be signalled durring logging

;;; Code:
(in-package :log)

(define-condition log-condition () ()
  (:documentation "Base class for all LOG-related conditions."))

(deferror log-error (log-condition) ()
  (:documentation "Base class for all LOG-related errors.") 
  (:auto t))

(deferror simple-log-error (log-error simple-error) () (:auto t))
          
