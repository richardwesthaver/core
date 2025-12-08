;;; math.lisp --- Math Top-level

;; 

;;; Code:
(in-package :math-int)

(defpkg :math 
  (:use :std-lisp)
  (:use-reexport . #.*math-packages*))
  
(defpkg :math-user
  (:use :std-lisp :tensor))
