;; log/err.lisp --- errors which may be signalled durring logging

;;; Code:
(in-package :log)

(eval-always
  (define-condition log-condition () ()
    (:documentation "Base class for all LOG-related conditions.")))

(deferror log-error (log-condition error) ()
  (:documentation "Base class for all LOG-related errors.") 
  (:auto t))

(deferror simple-log-error (log-condition simple-error) () (:auto t))
          
