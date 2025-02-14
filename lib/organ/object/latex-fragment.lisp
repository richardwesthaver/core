;;; lib/organ/object/latex-fragment.lisp --- Org Latex Fragment Object

;; Latex fragments match the patterns:

#|
\NAME BRACKETS
\(CONTENTS\)
\[CONTENTS\]
|#

;; The fun doesn't end here though, Org also supports inline latex
;; fragments matching:

#|
$$CONTENTS$$
PRE$CHAR$POST
PRE$BORDER1 BODY BORDER2$POST
|#

;; We define these as separate objects.

;;; Code:
(in-package :organ)

(define-org-object latex-fragment (name contents))

(define-org-object inline-latex-fragment (contents))
