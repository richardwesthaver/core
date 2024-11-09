;;; pkg.lisp --- Code Generation Packages

;; Codegen Packages

;;; Commentary:

;; The SYN/GEN system contains code generators for all major languages we use - including other Lisps and Common Lisp itself.

;;; Code:
(defpackage :syn/gen
  (:use :cl :std :doc :obj/id :obj/graph :dat/sxp)
  (:export :gen-designator :gen-condition :gen-condition
   :simple-gen-error))

(in-package :syn/gen)

;; TODO 2024-10-20: gen-file-header
