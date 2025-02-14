;;; lib/organ/element/greater/table.lisp --- Org Table Elements

;; Tables are started by a line beginning with either:

;; - a vertical bar '|', designating an 'org' type table.

;; - the string '+-' followed by a sequence of plus '+' and minus
;; - signs '-' forming a 'table.el' table type.

;; example
#|
| Name  | Phone | Age |
|-------+-------+-----|
| Peter |  1234 |  24 |
| Anna  |  4321 |  25 |
|#

;;; Code:
(in-package :organ)

(define-org-element table (rows) :greater t)

(define-org-element table-el (rows) :greater t)
