;;; util.lisp --- Math Utilities

;; Mostly internal utilities.

;;; Code:
(in-package :math/util)

(defun blasfunc (name type)
  `(function ,(intern (string-upcase (obj/tensor::blas-func name type)) :blas)))

(defun lapackfunc (name type)
  `(function ,(intern (string-upcase (obj/tensor::blas-func name type)) :lapack)))

#+nil
(deft/generic (t.blas-swap! #'subtypep) sym (x st-x y st-y))
#+nil
(deft/method t.blas-swap! (sym blas-mixin) (x st-x y st-y)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (x y))
      `(let (,@decl)
         (declare (type ,sym ,x ,y))
         (ffuncall ,(blas-func "swap" ftype)
                   (:& :int) (total-size ,y)
                   (:* ,(element-type-to-alien ftype) :+ (head ,x)) (the ,(store-type sym) (store ,x)) (:& :int) ,st-x
                   (:* ,(element-type-to-alien ftype) :+ (head ,y)) (the ,(store-type sym) (store ,y)) (:& :int) ,st-y)
         ,y))))

;;; Floating-point Simplification
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
