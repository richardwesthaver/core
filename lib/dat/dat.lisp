;;; dat.lisp --- Dat Top-Level

;; 

;;; Code:
(in-package :std-user)

(pkg:defpkg :dat
  (:use :cl :std)
  (:use-reexport . #.dat-int::*dat-packages*))
