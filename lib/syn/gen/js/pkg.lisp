;;; js/pkg.lisp --- Javascript Code Generator

;; Simple JS Codegen

;;; Commentary:

;; The current state-of-the-art in CL is Parenscript.

;; ref: https://parenscript.common-lisp.dev
;;; Code:
(defpackage :syn/gen/js
  (:nicknames :genjs :js)
  (:use :cl :syn/gen))

(in-package :syn/gen/js)
