;;; lib/obj/tree/bro.lisp --- Brother Tree

;; support for SBCL's 1-2 brother tree implementation.

;; ref: https://www.cs.ox.ac.uk/ralf.hinze/publications/Brother12.pdf

;; in general these are better than redblack trees

;; An element of type Tree T is called a brother tree iff:

;;   - a :: all nullary nodes have the same depth (height condition)

;;   - b :: each unary node has a binary brother (brother condition)

;; This implies that the root of a brother tree is not unary and that
;; a unary node has not a unary child. Put positively, a unary node
;; only occurs as the child of a binary node. 

;; -- Ralf Hinze, Purely Functional 1-2 Brother Trees

;;; Code:
(in-package :obj/tree)
