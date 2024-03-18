;;; obj/equiv.lisp --- Object Equivalence

;; extended equivalence API for Lisp objects

;;; Refs:

;; https://en.wikipedia.org/wiki/E-graph

;; https://gitlab.com/fstamour/catchall/-/tree/master/egraph

;; https://en.wikipedia.org/wiki/Disjoint-set_data_structure

;; https://clojure.org/guides/equality

;;; Code:
(in-package :obj/equiv)

(defgeneric equiv (a b))

(defgeneric eqv (a b))

(defgeneric nequiv (a b))

(defgeneric neqv (a b))
