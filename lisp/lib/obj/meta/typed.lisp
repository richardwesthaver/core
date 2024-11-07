;;; obj/meta/typed.lisp --- Typed meta-objects

;; - typed-slot-class

;; inspired by:
;; https://allegrograph.com/fixed-indices-speed-up-slot-access-in-allegro-cl/

;; may implement fixed.lisp separately.. we'll see.

;;; Commentary:

;; I still need to investigate what the actual behavior is in
;; SBCL.

;; - What sort of type checking is performed on slot-access, when that
;;   slot has type information? Does this vary at different compile levels?

;; - What is the performance impact of injecting additional
;;   slot-accessor type information? For example, declare as
;;   function-type with a typed result.

;;; Code:
(in-package :obj/meta/typed)

(defun type-num (obj)
  "Define a type order; no guarantee that backend and front-end match
   so we can't iterate over types, just all members of a give type class
   (i.e. numbers, etc)"
  (cond ((numberp obj) 1)
        ((characterp obj) 1)
        ((symbolp obj) 13)
        ((stringp obj) 2)
        ((subtypep (type-of obj) 'stored) 15)
        ((consp obj) 16)
        ((subtypep (type-of obj) 'standard-object) 18)
        ((pathnamep obj) 12)
        ((hash-table-p obj) 17)
        ((subtypep (type-of obj) 'structure-object) 20)
        ((complexp obj) 22)))

(defun type<= (obj1 obj2)
  (<= (type-num obj1) (type-num obj2)))

(defun type< (obj1 obj2)
  (< (type-num obj1) (type-num obj2)))

(defun type= (obj1 obj2)
  (= (type-num obj1) (type-num obj2)))
