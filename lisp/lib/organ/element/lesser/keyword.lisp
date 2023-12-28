;;; lib/organ/element/lesser/keyword.lisp --- Org Keyword Element

;; Keywords match the pattern '#+KEY: VALUE'

;; VALUE can be any of the standard-set of objects.

;; Affiliated keywords match the patterns:

#|
#+KEY: VALUE
#+KEY[OPTVAL]: VALUE
#+attr_BACKEND: VALUE
|#

;;; Code:
(in-package :organ)

(define-org-element keyword (key value) :lesser t)

(define-org-parser (keyword :from string))

(define-org-element affiliated-keyword (key opt value) :lesser t)

(define-org-parser (affiliated-keyword :from string))
