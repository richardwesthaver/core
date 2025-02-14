;;; lib/organ/element/lesser/latex-env.lisp --- Org Latex Environment Element

;; Latex environment elements match the pattern:

#|
\begin{NAME}
CONTENTS
\end{NAME}
|#

;;; Code:
(in-package :organ)

(define-org-element latex-environment (name contents) :lesser t)
