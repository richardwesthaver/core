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
