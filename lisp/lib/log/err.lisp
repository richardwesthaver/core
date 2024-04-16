;; log/err.lisp --- errors which may be signalled durring logging

;;; Code:
(in-package :log)

(deferror log-error (std-error simple-error program-error) ()
  (:documentation "Base class for all LOG-related errors."))
