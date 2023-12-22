;;; lib/obj/graph.lisp --- Graphs

;;

;;; Code:
(in-package :obj/graph)

;;; Vertex
(defclass vertex (node id)
  ()
  (:documentation "generic vertex mixin. The difference between this class and NODE is
that a vertex always carries an ID slot."))

;;; Edge
(defclass edge (node id)
  ()
  (:documentation "generic edge mixin. Compatible with the NODE and ID protocols."))

(defclass directed-edge (edge)
  ())

(defclass undirected-edge (edge)
  ())

(defclass weighted-edge (edge)
  ((weight :initform 1d0 :initarg :weight :accessor weight-of)))

(defgeneric edge-weight (edge &key &allow-other-keys)
  (:method ((edge t) &key &allow-other-keys) (values 1.0)))

;;; Graph
(defclass graph ()
  ()
  (:documentation "generic graph mixin."))
