;;; axpy.lisp --- AXPY BLAS Implementation

;; 

;;; Code:
(in-package :math/blas)

(deft/generic (t.blas-axpy! #'subtypep) sym (a x st-x y st-y))
(deft/method t.blas-axpy! (sym blas-mixin) (a x st-x y st-y)
  (let ((apy? (null x)) (ftype (field-type sym)))
    (using-gensyms (decl (a x y) (sto-x))
      `(let (,@decl)
         (declare (type ,sym ,@(unless apy? `(,x)) ,y)
                  (ignorable ,x ,a))
         ,(recursive-append
           (when apy? `(with-field-element ,sym (,sto-x (t.fid* ,ftype))))
           `(,(blas-func "axpy" ftype)
             (the index-type (total-size ,y))
              (the ,(field-type sym) ,a)
              ,(if apy? sto-x `(t.store ,sym ,x))
              (the index-type ,(if apy? 0 st-x))
              (t.store ,sym ,y)
              (the index-type ,st-y)))
         ,y))))

(deft/generic (t.axpy! #'subtypep) sym (a x y))
(deft/method t.axpy! (sym dense-tensor) (a x y)
  (let ((apy? (null x)))
    (using-gensyms (decl (a x y) (idx ref-x ref-y))
      `(let (,@decl)
         (declare (type ,sym ,@(unless apy? `(,x)) ,y)
                  (type ,(field-type sym) ,a)
                  (ignorable ,x ,a))
         (with-optimization (:speed 3 :safety 0)
           (dorefs (,idx (dimensions ,y))
                   (,@(unless apy? `((,ref-x ,x :type ,sym)))
                      (,ref-y ,y :type ,sym))
                   (setf ,ref-y (t.f+ ,(field-type sym) ,@(if apy? `(,a) `((t.f* ,(field-type sym) ,a ,ref-x))) ,ref-y))))
         ,y))))
;;---------------------------------------------------------------;;
(eval-always
  (defgeneric axpy! (alpha x y)
    (:documentation
     " 
 Syntax
 ======
 (AXPY! alpha x y)

 Y <- alpha * x + y

 If x is T, then

 Y <- alpha + y

 Purpose
 =======
  Same as AXPY except that the result
  is stored in Y and Y is returned.
")
    (:generic-function-class tensor-method-generator)))

(defmethod axpy! :before ((alpha number) (x base-tensor) (y base-tensor))
  (assert (vector-eq (dimensions x) (dimensions y) #'=) nil 'tensor-dimension-mismatch))

(define-tensor-method axpy! (alpha (x dense-tensor :y) (y dense-tensor :y t))
  `(let ((alpha (t.coerce ,(field-type (cl :y)) alpha)))
     (declare (type ,(field-type (cl :y)) alpha))
     ,(if (subtypep (cl :y) 'blas-mixin)
          `(let ((strd (and (call-fortran? y (t.blas-threshold ,(cl :y) 1)) (blas-copyablep x y))))
             (if strd
                 (t.blas-axpy! ,(cl :y) alpha x (first strd) y (second strd))
                 (t.axpy! ,(cl :y) alpha x y)))
          `(t.axpy! ,(cl :y) alpha x y))
     y))

(define-tensor-method axpy! (alpha x (y dense-tensor :y t))
  `(let ((alpha (t.coerce ,(field-type (cl :y)) alpha)))
     (declare (type ,(field-type (cl :y)) alpha))
     (when x (setq alpha (t.f* ,(field-type (cl :y)) alpha (t.coerce ,(field-type (cl :y)) x))))
     (unless (t.f= ,(field-type (cl :y)) alpha (t.fid+ ,(field-type (cl :y))))
       ,(if (subtypep (cl :y) 'blas-mixin)
            `(let ((strd (and (call-fortran? y (t.blas-threshold ,(cl :y) 1)) (consecutive-storep y))))
               (if strd
                   (t.blas-axpy! ,(cl :y) alpha nil nil y strd)
                   (t.axpy! ,(cl :y) alpha nil y)))
            `(t.axpy! ,(cl :y) alpha nil y)))
     y))
;;
(defgeneric axpy (alpha x y)
  (:documentation
   "
 Syntax
 ======
 (AXPY alpha x y)

 Purpose
 =======
 Computes  

                 ALPHA * X + Y

 where ALPHA is a scalar and X,Y are
 tensors.

 The result is stored in a new matrix 
 that has the same dimensions as Y.

 X,Y must have the same dimensions.
")
  (:method (alpha x (y dense-tensor))
    (axpy! alpha x (tensor-copy y (when (or (complexp alpha) (complexp x) (clinear-storep (type-of x)))
                             (complexified-tensor (type-of y)))))))
