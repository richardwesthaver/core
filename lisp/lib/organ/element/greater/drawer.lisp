;;; lib/organ/element/greater/drawer.lisp --- Org Drawer Elements

;; Drawers match the pattern:

#|
:NAME:
CONTENTS
:end:
|#

;; Drawers can't be nested.

;;; Code:
(in-package :organ)

(define-org-element drawer (name contents) :greater t)
