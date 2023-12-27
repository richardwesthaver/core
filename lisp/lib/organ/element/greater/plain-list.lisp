;;; lib/organ/element/greater/plain-list.lisp --- Org Plain List Element

;; Plain lists are a set of consecutive ORG-ITEM elements with the
;; same indentation. Items can of course contain other lists.

;; There are technically three classes of list: ordered, unordered,
;; and descriptive.

;;; Code:
(in-package :organ)

(define-org-element ordered-list (items) :greater t)

(define-org-element unordered-list (items) :greater t)

(define-org-element descriptive-list (items) :greater t)
