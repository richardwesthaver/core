;;; lib/obj/graph.lisp --- Graphs

;;

;;; Code:
(in-package :obj/graph)

(defclass node () ())

;;; Vertex
(defclass vertex (id node)
  ()
  (:documentation "generic vertex mixin. The difference between this class and NODE is
that a vertex always carries an ID slot."))

;;; Edge
(defclass edge (id node)
  (a b)
  (:documentation "generic edge mixin. Compatible with the NODE and ID protocols."))

(defclass directed-edge (edge)
  (a b)
  (:documentation "An edge with an implicit direction from node A to B."))

(defclass weighted-edge (edge)
  ((weight :initform 1d0 :initarg :weight :accessor weight-of)))

(defgeneric edge-weight (edge &key &allow-other-keys)
  (:method ((edge t) &key &allow-other-keys) (values 1.0)))

;;; Graph
(defclass graph (node)
  ()
  (:documentation "generic graph mixin."))
