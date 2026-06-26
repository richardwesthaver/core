;;; lib/organ/object/footnote-ref.lisp --- Org Footnote Reference Object

;; See [[https://orgmode.org/guide/Creating-Footnotes.html][Creating Footnotes]]

#|
[fn:LABEL]
[fn:LABEL:DEFINITION]
[fn::DEFINITION]
|#

;;; Code:
(in-package :organ)

(define-org-object footnote-reference (label definition))
