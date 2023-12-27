;;; lib/organ/object/footnote-ref.lisp --- Org Footnote Reference Object

;; Footnote references matches the following patterns:

#|
[fn:LABEL]
[fn:LABEL:DEFINITION]
[fn::DEFINITION]
|#

;;; Code:
(in-package :organ)

(define-org-object footnote-reference (label definition))
