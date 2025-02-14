;;; lib/organ/element/greater/inlinetask.lisp --- Org Inline Task Element

;; Inlinetasks are kinda weird - they're syntactically a heading with
;; a level of at least *ORG-INLINETASK-MIN-LEVEL*. Usually this means
;; a lot of stars.

;; You can nest elements inside an inlinetask by adding a second
;; heading at the same level with a title of 'END' and no other
;; syntax. Otherwise the inlinetask is a unit and won't consume the
;; contents on the following lines.

;;; Code:
(in-package :organ)

;; TODO
(define-org-element inlinetask (headline contents) :greater t)
