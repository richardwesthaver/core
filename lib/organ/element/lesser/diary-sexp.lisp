;;; lib/organ/element/lesser/diary-sexp.lisp --- Org Diary Sexp Element

;; Diary sexps are unindented lines that match the pattern: '%%SEXP'.

;;; Code:
(in-package :organ)

(define-org-element diary-sexp (sexp) :lesser t)
