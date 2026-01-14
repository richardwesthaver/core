;;; obj/equiv.lisp --- Object Equivalence

;; extended equivalence API for Lisp objects

;;; Commentary:

;; A valid complaint of Common Lisp is the lack of an intuitive
;; equality mechanism. We have the following symbols in the
;; standard. If you want to write CL you need to have a very firm
;; grasp on these:

#|
= EQ EQL EQUAL EQUALP TREE-EQUAL
|#

;; EQUALP is the most flexible, but it is still explicitly conforming
;; to the spec. It does handle STRUCTURE objects as one might expect,
;; but it will not handle CLASSes. 

;; As a rule, don't mess with these (i.e. redefine them, or anything
;; in the standard for that matter) and use them wherever
;; possible. Anything that is testing equality between two lisp
;; objects is very likely to boil down to one of the symbols above,
;; and the compiler handles the rest.

;; For simplicity, let us consider these symbols provided by the
;; standard to be of the EQUALITY category.

;; This package in essence provides a superset of this, called the
;; EQUIVALENCE category. These functions are generic and designed to
;; be implemented on user-defined objects with support for highly
;; complex relationships, partial equality, and more.

;; Here are some things to keep in mind for this potion of code:

;; - I'm not as familiar with Clojure (which has equiv) and JVM
;;   semantics as much as I'd like to be on this topic.

;; - I believe that Rust got it right for the most part. PartialEq and
;;   Eq are the two equality traits, with PartialOrd and Ord being the
;;   ordering traits which are a natural extension to equality. In
;;   turn the MAX and MIN functions build off of Ord (total
;;   order). The APIs that follow (order.lisp,limit.lisp) will be
;;   designed with these relationships in mind.

;;; Refs:

;; https://en.wikipedia.org/wiki/E-graph

;; https://en.wikipedia.org/wiki/Equivalence_relation

;; https://doc.rust-lang.org/std/cmp/index.html

;; https://gitlab.com/fstamour/catchall/-/tree/master/egraph

;; https://en.wikipedia.org/wiki/Disjoint-set_data_structure

;; https://clojure.org/guides/equality

;;; Code:
(in-package :obj/equiv)

;; EQUALS > EQUIV > EQV
(defgeneric equiv (a b)
  (:method ((a character) (b string))
    (and (= 1 (length b)) (char= (aref b 0) a)))
  (:method ((a string) (b character))
    (equiv b a))
  (:method ((a string) (b string))
    (string= a b))
  (:method ((a t) (b t))
    (equal a b)))

(defgeneric eqv (a b))

(defgeneric equals (a b &rest args))
