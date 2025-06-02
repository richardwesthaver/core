;;; prim.lisp --- Lisp Primitives

;; 

;;; Code:
(in-package :math)

(defconstant +epsilon+ 1.e-7
  "Used as a liminal value to work around floating point inaccuracy.")

(defconstant +pi+ (coerce pi 'single-float)
  "Single-float PI.")

(declaim (inline ~))
(defun ~ (a b &optional (epsilon +epsilon+))
  "Return true if A and B are within EPSILON of each other. EPSILON
defaults to +DEFAULT-EPSILON+."
  (< (- epsilon) (- a b) epsilon))

;;; Open code comparisons to constants: no substraction needed at runtime.
(define-compiler-macro ~ (&whole form a b &optional (epsilon +epsilon+))
  (if (constantp epsilon)
      (flet ((open-code (x constant)
               (let ((c (eval constant))
                     (e (eval epsilon)))
                 `(< ,(- c e) ,x ,(+ c e)))))
        (cond ((constantp a)
               (open-code b a))
              ((constantp b)
               (open-code a b))
              (t
               form)))
      form))
