;;; lib/organ/element/greater/block.lisp --- Org Greater Block Elements

;; Dynamic blocks match the pattern:

#|
#+begin NAME PARAMETERS
CONTENTS
#+end
|#

;;; Code:
(in-package :organ)

(define-org-element dynamic-block 
    ((name :initarg :name :accessor name) 
     (parameters :initarg :parameters) 
     (contents :initarg :contents :accessor org-contents))
  :greater t)
