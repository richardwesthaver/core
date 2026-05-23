;;; lib/obj/node.lisp --- Node Structures

;; Basic Tree Node Structs

;;; Code:
(in-package :obj/node)

(deftype keytype () 'sb-vm:word)

(defstruct (tree-node (:copier nil)
                 (:constructor make-tree-node (key)))
  (key 0 :type keytype))

(defstruct (unary-node (:include tree-node))
  (child nil :type t))

(defstruct (binary-node (:include tree-node)
                       (:copier nil)
                       (:constructor make-binary-node (key left right)))
  left right)

;; temporary nodes eliminated when a tree is compiled
(defstruct (ternary-node (:include binary-node)
                         (:copier nil)
                         (:constructor make-ternary-node (left key1 middle key2 right)))
  key1 middle key2)

(defstruct (avl-node (:include tree-node)
                     (:copier nil)
                     (:constructor make-avl-node (key data left right)))
  data left right)

