;;; lib/syn/pkg.lisp --- Syn Packages

;; Syntax Processors

;;; Commentary:

;;; Code:
(defpackage :syn
  (:use :cl :std :obj :parse :tree-sitter)
  (:export
   #:syntax))

(defpackage :syn/lint
  (:use :cl :std :syn)
  (:export :lint))

(defpackage :syn/ts
  (:use :cl :std :syn :tree-sitter)
  (:export :lint))

(defpackage :syn/lang
  (:use :cl :std :syn)
  (:export :language :lang))

(in-package :syn)
(defclass syntax () ())

(in-package :syn/lang)
(defclass language () ())
(defgeneric lang (self))
