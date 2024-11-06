;;; lib/obj/tree/redblack.lisp --- Red-Black Tree

;; support for SBCL's purely functional red/black tree implementation.

;; ref: https://www.seas.upenn.edu/~cis552/13fa/lectures/RedBlack.html

;;; Code:
(defpackage :obj/tree/redblack
  (:nicknames :tree/rb :redblack :tree/redblack :obj/tree/rb)
  (:use :cl :std :obj/tree))
(in-package :obj/tree/redblack)
