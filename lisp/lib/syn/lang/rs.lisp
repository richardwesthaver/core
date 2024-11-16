;;; rs.lisp --- Rust Treesitter Parser

;; 

;;; Code:
(defpackage :syn/lang/rs
  (:nicknames :syn/rs)
  (:use :cl :std :syn/lang :tree-sitter)
  (:export))

(in-package :syn/rs)
(load-tree-sitter-rust)
