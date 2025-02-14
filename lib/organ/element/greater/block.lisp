;;; lib/organ/element/greater/block.lisp --- Org Greater Block Elements

;; Greater blocks match the pattern:

#|
#+begin_NAME PARAMETERS
CONTENTS
#+end_NAME
|#

;;; Code:
(in-package :organ)

(define-org-element greater-block (name parameters contents) :greater t)
