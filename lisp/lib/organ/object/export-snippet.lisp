;;; lib/organ/object/export-snippet.lisp --- Org Export Snippet Object

;; Export snippets match the pattern: @@BACKEND:VALUE@@

;;; Code:
(in-package :organ)

(define-org-object export-snippet (backend value))
