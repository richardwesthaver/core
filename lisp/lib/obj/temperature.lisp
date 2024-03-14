;;; obj/temperature.lisp --- Temperature objects and conversions

;; In most cases you're better off using the direct conversion
;; functions on literal numbers, but defining structs is good for
;; printing and other utility methods.

;;; Code:
(in-package :obj/temperature)

(defvar *default-temperature-unit* :fahrenheit)

(deftype temperature-unit-designator () '(member :fahrenheit :celsius :kelvin :rankine))

(defstruct temperature
  (degrees 0 :type single-float)
  (scale *default-temperature-unit* :type temperature-unit-designator))
