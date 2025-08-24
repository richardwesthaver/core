;;; obj.lisp --- OBJ Top-level

;; 

;;; Code:
(in-package :std-user)

(defpkg :obj
  (:use :cl :std)
  (:use-reexport . #.obj/int:*obj-packages*))

