;;; js.lisp --- Javascript Treesitter Parser

;; 

;;; Code:
(defpackage :syn/lang/js
  (:nicknames :syn/js)
  (:use :cl :std :syn/lang :tree-sitter)
  (:export))

(in-package :syn/lang/js)
(load-tree-sitter-javascript)
