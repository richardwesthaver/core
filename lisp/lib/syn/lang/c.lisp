;;; c.lisp --- C Syntax

;; 

;;; Code:
(defpackage :syn/lang/c
  (:nicknames :syn/c)
  (:use :cl :std :syn/lang :parse/pratt :tree-sitter)
  (:export))

(in-package :syn/lang/c)

(load-tree-sitter-c)
(tree-sitter-c)
