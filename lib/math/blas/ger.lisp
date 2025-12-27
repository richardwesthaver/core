;;; ger.lisp --- General Matrix Rank-1 update

;; 

;;; Code:
(in-package :math/blas)

(deft/generic (t.blas-ger! #'subtypep) sym (alpha x st-x y st-y A lda &optional conjp))
(deft/method t.blas-ger! (sym blas-mixin) (alpha x st-x y st-y A lda &optional (conjp t))
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (alpha x st-x y st-y A lda) (m n))
      `(let* (,@decl
              (,m (dimensions ,A 0)) (,n (dimensions ,A 1)))
         (declare (type ,sym ,A ,x ,y)
                  (type ,(field-type sym) ,alpha)
                  (type index-type ,st-x ,st-y ,lda ,m ,n))
         (,(blasfunc (concatenate 'string "ger" (when (subtypep ftype 'complex) (if conjp "c" "u"))) ftype)
          ,m ,n
          ,alpha
          (deref (the ,(store-type sym) (store ,x)) (head ,x))
          (the index-type ,st-x)
          (deref (the ,(store-type sym) (store ,y)) (head ,y))
          (the index-type ,st-y)
          (deref (the ,(store-type sym) (store ,A)) (head ,A))
          ,lda)
         ,A))))

(deft/generic (t.ger! #'subtypep) sym (alpha x y A &optional conjp))
(deft/method t.ger! (sym dense-tensor) (alpha x y A &optional (conjp t))
  (using-gensyms (decl (alpha A x y))
   `(let (,@decl)
      (declare (type ,sym ,A ,x ,y)
               (type ,(field-type sym) ,alpha))
      ;;These loops are optimized for column major matrices
      (unless (t.f= ,(field-type sym) ,alpha (t.fid+ ,(field-type sym)))
        (einstein-sum ,sym (j i) (ref ,A i j) (* ,alpha (ref ,x i)
                                                 ,(recursive-append
                                                   (when conjp `(t.fc ,(field-type sym)))
                                                   `(ref ,y j)))
                      nil))
      ,A)))

(eval-always
  (defgeneric ger! (alpha x y A &optional conjugate-p)
    (:documentation
     "Performs the GEneral matrix Rank-1 update given by

A <- alpha * x * op(y) + A

and returns A.

alpha is a scalars,
x,y are vectors.
A is a matrix.

If conjugate-p is nil, then op(y) = y^T, else op(y) = y^H.")
    (:method :before (alpha (x dense-tensor) (y dense-tensor) (A dense-tensor) &optional conjugate-p)
      (declare (ignore conjugate-p))
      (assert (and
               (tensor-vectorp x) (tensor-vectorp y) (tensor-matrixp A)
               (= (dimensions x 0) (dimensions A 0))
               (= (dimensions y 0) (dimensions A 1)))
              nil 'tensor-dimension-mismatch))
    (:generic-function-class tensor-method-generator)))

(define-tensor-method ger! (alpha (x dense-tensor :x) (y dense-tensor :x) (A dense-tensor :A) &optional (conjugate-p t))
  `(let ((alpha (t.coerce ,(field-type (cl :x)) alpha)))
     (declare (type ,(field-type (cl :x)) alpha))
     ,(recursive-append
       (when (subtypep (cl :x) 'blas-mixin)
         `(if (call-alien-p A (t.blas-lb ,(cl :a) 2))
              (with-columnification (() (A))
                (if conjugate-p
                    (t.blas-ger! ,(cl :a) alpha x (strides x 0) y (strides y 0) A (or (blas-matrix-compatiblep A #\N) 0) t)
                    (t.blas-ger! ,(cl :a) alpha x (strides x 0) y (strides y 0) A (or (blas-matrix-compatiblep A #\N) 0) nil)))))
       `(if conjugate-p
            (t.ger! ,(cl :a) alpha x y A t)
            (t.ger! ,(cl :a) alpha x y A nil))))
  'A)

(defgeneric ger (alpha x y A &optional conjugate-p)
  (:documentation
   "Performs the GEneral matrix Rank-1 update given by
             --             -

           alpha * x * op(y) + A

and returns A.

alpha is a scalars,
x,y are vectors.
A is a matrix.

If conjugate-p is nil, then op(y) = y^T, else op(y) = y^H."))

(defmethod ger (alpha (x dense-tensor) (y dense-tensor) (A dense-tensor) &optional conjugate-p)
  (ger! alpha x y (tensor-copy A) conjugate-p))

(defmethod ger (alpha (x dense-tensor) (y dense-tensor) (A (eql nil)) &optional conjugate-p)
  (ger! alpha x y (zeros (append (dimensions x t) (dimensions y t))) conjugate-p))

;; late definition of tensor/map function (requires GER!)
(defun meshgrid (a b)
  (declare (type tensor-vector a b))
  (let ((x (zeros (list (dimensions a 0) (dimensions b 0)) (class-of a)))
        (y (zeros (list (dimensions a 0) (dimensions b 0)) (class-of a))))
    (ger! 1 a (ones (dimensions b 0) (class-of b)) x)
    (ger! 1 (ones (dimensions a 0) (class-of a)) b y)
    (values x y)))
