;;; obj/direction.lisp

;;

;;; Commentary:

;;; Code:
(in-package :obj/unit)

;;; Direction
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

;;; Temperature
(defvar *default-temperature-unit* :fahrenheit)

(deftype temperature-unit-designator () '(member :fahrenheit :celsius :kelvin :rankine))

(defstruct temperature
  (degrees 0 :type single-float)
  (scale *default-temperature-unit* :type temperature-unit-designator))
