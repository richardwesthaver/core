;;; proto.lisp --- Math Conditions

;; 

;;; Code:
(in-package :math/proto)

(define-condition math-condition () ())
(deferror math-error (math-condition error) () (:auto t))
(defwarning math-warning (math-condition warning) () (:auto t))
