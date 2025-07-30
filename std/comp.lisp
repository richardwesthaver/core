;;; comp.lisp --- Lisp Compiler Utilities

;; 

;;; Code:
(in-package :std/comp)

(definline primitive-type-name-of (obj)
  (primitive-type-name (primitive-type-of obj)))

(defun backend-primitive-type (name)
  (gethash name *backend-primitive-type-names*))
