;;; lib/organ/element/lesser/block.lisp --- Org Lesser Block Elements

;; Lesser blocks are structured similar to greater blocks:

#|
#+begin_NAME DATA
CONTENTS
#+end_NAME
|#

;;; Code:
(in-package :organ)

(define-org-element lesser-block (name data contents) :lesser t)
