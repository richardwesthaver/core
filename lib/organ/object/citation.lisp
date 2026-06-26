;; This file covers citations and citation references.

;; See [[https://www.gnu.org/software/emacs/manual/html_node/org/Citations.html][Citations]]

;; Citation references match the following patterns:

;; KEYPREFIX @KEY KEYSUFFIX

;;; Code:
(in-package :organ)

(define-org-object citation (style pfx refs sfx))

(define-org-object citation-reference (pfx key sfx))
