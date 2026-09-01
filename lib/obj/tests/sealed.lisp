;;; sealed.lisp --- Sealed Metaclass Tests

;; 

;;; Code:
(in-package :obj/tests)

(defgeneric %test-+ (a b)
  (:generic-function-class fast-generic-function))

(defmethod %test-+ ((a number) (b number))
  (+ a b))

(seal-domain #'%test-+ '(number number))
