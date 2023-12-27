;;; lib/organ/element/lesser/table-row.lisp --- Org Table Row Object

;; Table rows consists of a vertical bar '|' followed by:

;; - Any number of ORG-TABLE-CELL objects forming a 'standard' row.

;; -- A hyphen '-' forming a 'rule' type row. Any characters can
;;    follow the hyphen.

;; Table rows can only exist within a table.

;;; Code:
(in-package :organ)

(define-org-element standard-table-row (cells) :lesser t)

(define-org-element rule-table-row () :lesser t)
