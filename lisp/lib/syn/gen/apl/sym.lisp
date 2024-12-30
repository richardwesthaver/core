;;; sym.lisp --- APL Symbols

;; 

;;; Code:
(in-package :apl/sym)

(defmacro ⍝ (&rest args)
  `(make-instance 'comment :comment ,(format nil "~{~a~^ ~}" args) :chars "⍝"))
