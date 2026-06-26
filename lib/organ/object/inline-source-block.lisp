;; Inline source blocks match the pattern:

;; See [[https://orgmode.org/manual/Structure-of-Code-Blocks.html][Structure of Code Blocks]]

;;; Code:
(in-package :organ)

(define-org-object inline-source-block (lang headers body))
