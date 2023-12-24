;;; lib/obj/tree/avl.lisp --- AVL Tree

;; O(log n) for basic ops

;; similar to Redblack Tree but generally faster for lookup-intensive
;; workloads. https://en.wikipedia.org/wiki/AVL_tree

;;; Code:
(in-package :obj/tree)
