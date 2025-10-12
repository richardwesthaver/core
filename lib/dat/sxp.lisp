;;; sxp.lisp --- S-Expression Serializers

;; parens-less S-expression coding

;;; Commentary:

;; We have gone back and forth several times on whether this module should
;; bother to exist, considering the obvious and deep integration between
;; S-Expressions and Lisp. The reason why we bother with it today is partially
;; to tie in the OBJ/AST protocol with SERDE and DAT but mostly to provide
;; a space /specifically/ for the SXP representation.

;; One thing to note is that if we didn't bother with the line-based
;; representation (SXL), we likely wouldn't need this module.

;;; Code:
(in-package :dat/sxp)

;;; SXP
;; parse input as elements of a pre-allocated list.

;;; SXL
;; parse each line as SXP into a pre-allocated list.
