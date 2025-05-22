;;; select.lisp --- Simple Selection

;; Trivial, easily-embedded subscript-based queries.

;;; Commentary:

;; The idea here is to provide the most basic, easy to slap on query api as
;; possible. These can be added to any lisp collection or object to give it
;; queryable accessors which can be combined according to a QUERY-MODEL.

;; - boolean-predicate
;; - boolean-set

;;; Code:
(in-package :q/select)
