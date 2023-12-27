;;; lib/organ/element/lesser/comment.lisp --- Org Comment Element

;; This file covers 'comment lines' which are simply any line starting
;; with a hash char '#'.

;;; Code:
(in-package :organ)

(define-org-element comment (contents) :lesser t)
