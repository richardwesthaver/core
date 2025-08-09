;;; py.lisp --- Python

;; 

;;; Code:
(defpackage :syn/lang/py
  (:use :cl :std :syn/lang :tree-sitter :syn/ts)
  (:export))
(in-package :syn/lang/py)
(load-tree-sitter-python)

            
