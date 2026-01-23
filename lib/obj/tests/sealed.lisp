;;; sealed.lisp --- Sealed Metaclass Tests

;; 

;;; Code:
(in-package :obj/tests)

(defgeneric %test-+ (a b)
  (:generic-function-class fast-generic-function))
