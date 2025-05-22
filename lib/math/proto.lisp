;;; proto.lisp --- Math Core Protocols

;; 

;;; Code:
(in-package :math/core)

(define-condition math-condition () ())
(deferror math-error (math-condition error) () (:auto t))
(defwarning math-warning (math-condition warning) () (:auto t))
