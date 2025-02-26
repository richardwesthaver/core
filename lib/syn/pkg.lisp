;;; lib/syn/pkg.lisp --- Syn Packages

;; Syntax Processors

;;; Commentary:

;;; Code:
(defpackage :syn/lint
  (:use :cl :std)
  (:export :lint))

(defpackage :syn/ts
  (:use :cl :std :tree-sitter)
  (:export 
   :parse-file
   :lang-counts))

(defpackage :syn/lang
  (:use :cl :std)
  (:export :language :lang))

(defpackage :syn
  (:use :cl :std :syn/lint :syn/ts :syn/lang)
  (:export :syntax))

(defpackage :syn/cli
  (:use :cl :std :syn/lint :syn/ts :syn/lang :cli)
  (:export :*syn-cli*))

(in-package :syn)
(defclass syntax () ())

(in-package :syn/lang)
(defclass language () ())
(defgeneric lang (self))
