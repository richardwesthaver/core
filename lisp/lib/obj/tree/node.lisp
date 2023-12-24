;;; lib/obj/tree/node.lisp --- Tree Nodes

;; SBCL provides excellent support for the ANSI CL built-in types such
;; as vector/array, list, hash-table. We don't need to provide much in
;; terms of additional support for these. There are some internal SBCL
;; collections such as RBTREE, AVLTREE and BROTHERTREE which we expose here
;; and provide additional support for.

;;; Code:
(in-package :obj/tree)

(deftype keytype () 'sb-vm:word)

(defstruct (node (:copier nil)
                 (:constructor make-node (key)))
  (key 0 :type keytype))

(defstruct (unary-node (:include node))
  (child nil :type t))

(defstruct (binary-node (:include node)
                       (:copier nil)
                       (:constructor make-binary-node (key left right)))
  left right)

;; temporary nodes eliminated when a tree is compiled
(defstruct (ternary-node (:include binary-node)
                         (:copier nil)
                         (:constructor make-ternary-node (left key1 middle key2 right)))
  key1 middle key2)

(defstruct (avl-node (:include node)
                     (:copier nil)
                     (:constructor make-avl-node (key data left right)))
  data left right)

