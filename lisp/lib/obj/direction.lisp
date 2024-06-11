;;; obj/direction.lisp --- Physical and Metaphysical Directions

;;

;;; Commentary:

;;; Code:
(in-package :obj/direction)

(deftype direction-designator () '(or symbol string boolean number))

(defclass direction () ())

(defgeneric direction (self))
(defgeneric (setf direction) (self))

(defgeneric directions (self))
(defgeneric (setf directions) (self))

(defgeneric left (self))
(defgeneric right (self))
(defgeneric up (self))
(defgeneric down (self))
