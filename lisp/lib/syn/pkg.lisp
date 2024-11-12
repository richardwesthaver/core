;;; lib/syn/pkg.lisp --- Syn Packages

;; Syntax Processors

;;; Commentary:

;;; Code:
(defpackage :syn
  (:use :cl :std :obj :parse :tree-sitter)
  (:export))

(defpackage :syn/lint
  (:use :cl :std :syn)
  (:export :lint))

(in-package :syn)

(defvar *syntax-tree*)

(defclass syntax () ())

;; does not need tree-sitter lang loaded before use
(defclass tree-sitter-syntax (syntax)
  ((path :accessor path)
   info))

;; needs tree-sitter lang loaded before use
(defstruct tree-sitter-syntax-info
  version
  symbols
  fields)
