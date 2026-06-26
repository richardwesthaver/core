;;; lib/organ/object/macro.lisp --- Org Macro Object

;; See [[https://orgmode.org/manual/Macro-Replacement.html][Macro Replacement]]

;;; Code:
(in-package :organ)

(define-org-object macro (name args))

(define-org-parser (macro :from string)
  (let ((r (org-create :macro)))
    r))
