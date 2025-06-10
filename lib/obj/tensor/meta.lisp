;;; meta.lisp --- Tensor MOP

;; 

;;; Commentary:

;; [[id:0521332c-11d2-4ffc-8ada-99690b8b2655][dispatch strategy for tensor methods]]

;; DEFTENSOR - define a tensor object class

;; DEFT - define a tensor method

;;; Code:
(in-package :obj/tensor)

(defclass tensor-class (standard-class) ())

(defmacro deftensor (name supers slots &rest options)
  `(defclass ,name ,supers ,slots ,@options (:metaclass tensor-class)))
