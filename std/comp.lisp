;;; comp.lisp --- SBCL Compiler Utilities

;; 

;;; Code:
(in-package :std/comp)

(definline primitive-type-name-of (obj)
  (primitive-type-name (primitive-type-of obj)))
