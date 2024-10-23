;;; util.lisp --- Codegen Utilities

;; 

;;; Code:
(in-package :syn/gen)
;; FBOUNDP!, VBOUNDP! and QUOTY are all described in the C-MERA paper.
(defun fboundp! (function &optional env)
  "Check if function or macro is bound globally or lexically."
  (sb-cltl2::function-information function env))

(defun vboundp! (variable &optional env)
  "Check if variable or symbol macro is bound  globally or lexically."
  (sb-cltl2::variable-information variable env))

;;;; Quoty
(defmacro quoty (item &environment env)
  "Quote undefined symbols, build functions from unknown lists"
  (cond ((eql item nil)
         (values))
        ((listp item)
         (if (or (listp (first item))
                 (not (fboundp! (first item) env)))
             `(function-call
               (make-node ,(first item))
               (make-nodelist ,(rest item)))
             item))
        ((symbolp item)
         (if (vboundp! item env)
             item
             `',item))
        (t item)))
