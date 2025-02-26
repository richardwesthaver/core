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
  (:export :language :lang :*language*))

(defpackage :syn
  (:use :cl :std :syn/lint :syn/ts :syn/lang))

(defpackage :syn/cli
  (:use :cl :std :syn/lint :syn/ts :syn/lang :cli)
  (:export :*syn-cli*))

(in-package :syn)

(in-package :syn/lang)
(defclass language () ())
(defgeneric lang (self))
(sb-ext:define-load-time-global *language* nil)
