;;; lib/organ/object/table.cell.lisp --- Org Table Cell Object

;; Table cells match the pattern:

;; CONTENTS SPACES| or CONTENTS SPACES EOL

;; CONTENTS matches the minimal-set of objects.

;;; Code:
(in-package :organ)

(define-org-object table-cell (contents))
