;;; lib/organ/object/entity.lisp --- Org Entity Object

;; Entities match the following patterns:

#|
\NAME POST
\NAME{}
\_SPACES
|#

;;; Code:
(in-package :organ)

(define-org-object entity (name))
