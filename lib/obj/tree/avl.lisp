;;; lib/obj/tree/avl.lisp --- AVL Tree

;; O(log n) for basic ops

;; similar to Redblack Tree but generally faster for lookup-intensive
;; workloads. https://en.wikipedia.org/wiki/AVL_tree

;;; Code:
(defpackage :obj/tree/avl
  (:nicknames :tree/avl :avl)
  (:use :cl :std :obj/tree))
(in-package :obj/tree/avl)
