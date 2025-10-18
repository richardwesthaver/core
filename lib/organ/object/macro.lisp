;;; lib/organ/object/macro.lisp --- Org Macro Object

;; Macros come in the following patterns:

;; {{{NAME}}} or {{{NAME(ARGUMENTS)}}}

;;; Code:
(in-package :organ)

(define-org-object macro (name args))

(define-org-parser (macro :from string)
  (let ((r (org-create :macro)))
    r))
