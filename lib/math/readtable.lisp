;;; readtable.lisp --- Math Readtables

;; Standard Algebraic Notation (LR)

;;; Commentary:

;; We make use of the same LINPARSE implementation as MATLISP, dispatching to
;; our own machinery.

;;; Code:
(in-package :math/syn)

(macrolet ((tensor-symbol-enumerate ()
             `(defreadtable :tensor
                (:merge :std)
                ,@(mapcar #'(lambda (x) `(:dispatch-macro-char #\# ,(car x) #'tensor-reader)) *tensor-symbol*))))
  (tensor-symbol-enumerate))

(defreadtable :math
  (:merge :tensor)
  (:dispatch-macro-char #\# #\I #'infix-reader))
