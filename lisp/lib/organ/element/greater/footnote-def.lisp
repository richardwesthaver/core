;;; lib/organ/element/greater/footnote-def.lisp --- Org Footnote Definition Element

;; Footnote definitions match the pattern: '[fn:LABEL] CONTENTS'

;;; Code:
(in-package :organ)

(define-org-element footnote-definition (label contents) :greater t)
