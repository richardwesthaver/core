;;; lib/obj/coll.lisp --- Collections

;; The classes exported by this package are used to define new tree
;; collections. A collection is simply a data structure which stores
;; objects.

;; SBCL provides excellent support for the ANSI CL built-in types such
;; as vector/array, list, hash-table. We don't need to provide much in
;; terms of additional support for these. There are some internal SBCL
;; collections such as RBTREE, AVLTREE and BROTHERTREE which we expose here
;; and provide additional support for. The remainder of the code is
;; dedicated to custom implementations suited for specific use-cases.

;;; Code:
(in-package :obj/tree)

;;; Node
(defclass node ()
  ()
  (:documentation "generic node mixin."))

(defclass unary-node (node)
  (child))

(defclass binary-node (node)
  (left right))

;; temporary nodes eliminated when a tree is compiled
(defclass ternary-node (binary-node)
  (key1 middle key2))

;;; AVL Tree

;; O(log n) for basic ops

;; similar to Redblack Tree but generally faster for lookup-intensive
;; workloads. https://en.wikipedia.org/wiki/AVL_tree

(defclass avlnode (binary-node)
  (data))

;;; Brother Tree

;; support for SBCL's 1-2 brother tree implementation.

;; ref: https://www.cs.ox.ac.uk/ralf.hinze/publications/Brother12.pdf

;; in general these are better than redblack trees

;;; Redblack Tree

;; support for SBCL's purely functional red/black tree implementation.

;; ref: https://www.seas.upenn.edu/~cis552/13fa/lectures/RedBlack.html
