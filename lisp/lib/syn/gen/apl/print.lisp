;;; print.lisp --- SYN/GEN/APL Code Printer

;; 

;;; Commentary:

;; Unlike our other GEN code printers, this one is really intended for debug
;; purposes only, where the internal AST needs to be inspected.

;; Instead of printing, APL expressions should be processed by an
;; APL-EVALUATOR together with an APL-ENVIRONMENT which computes the result
;; and returns output to the user.

;;; Code:
(in-package :syn/gen/apl)
