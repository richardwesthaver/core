;;; trs.lisp --- TRS

;; 

;;; Code:
(in-package :math/blas)

(deft/generic (t.blas-trsm! #'subtypep) sym (side uplo transA diagA alpha A lda B ldb))
(deft/method t.blas-trsm! (sym blas-mixin) (side uplo transA diagA alpha A lda B ldb)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (side uplo transA diagA alpha A lda B ldb))
      `(let* (,@decl)
         (declare (type ,sym ,A ,B)
                  (type ,(field-type sym) ,alpha)
                  (type index-type ,lda ,ldb)
                  (type character ,transa ,diaga ,uplo ,side))
         (,(blas-func "trsm" ftype)
          ,side ,uplo ,transa ,diagA
          (dimensions ,B 0) (dimensions ,B 1)
          ,alpha
          (deref (the ,(store-type sym) (store ,A)) (head ,A)) ,lda
          (deref (the ,(store-type sym) (store ,B)) (head ,B)) 
          ,ldb)
         ,B))))

(deft/generic (t.blas-trsv! #'subtypep) sym (uplo transA diagA A lda b st-b))
(deft/method t.blas-trsv! (sym blas-mixin) (uplo transA diagA A lda b st-b)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (uplo transA diagA A lda b st-b))
      `(let* (,@decl)
         (declare (type ,sym ,A ,b)
                  (type index-type ,lda ,st-b)
                  (type character ,transa ,diaga ,uplo))
         (,(blas-func "trsv" ftype)
           ,uplo ,transa ,diagA
           (dimensions ,A 0)
           (deref (the ,(store-type sym) (store ,A)) (head ,A))
          ,lda
           (deref (the ,(store-type sym) (store ,B)) (head ,B))
          ,st-b)
         ,b))))
;;
(eval-always
  (defgeneric trs! (alpha A b &optional solve uplo)
    (:documentation "Solve -> :{trans-> n/t/c}{Diag-> u/n}{Side-> l/r}")
    (:method :before (alpha (A dense-tensor) (b dense-tensor) &optional (solve :nn) (uplo *default-uplo*))
      (destructuring-bind (joba diag &optional (side #\L sidep)) (split-job solve)
        (assert (and (member joba '(#\N #\T #\C) :test #'char=) (member side '(#\L #\R) :test #'char=) (member diag '(#\U #\N) :test #'char=)
                     (member uplo '(:u :l))) nil 'invalid-arguments)
        (assert (and (typep A 'tensor-square-matrix) (if sidep (tensor-matrixp b) (<= (order b) 2))
                     (= (dimensions A 0) (dimensions B (ecase side (#\L 0) (#\R 1))))) nil 'tensor-dimension-mismatch)))
    (:generic-function-class tensor-method-generator)))

(define-tensor-method trs! (alpha (A blas-mixin :x) (b blas-mixin :x) &optional (solve :nn) (uplo *default-uplo*))
  `(destructuring-bind (joba diag &optional (side #\L)) (split-job solve)
     (with-columnification (((A joba)) ())
       (letv* ((lda opa (blas-matrix-compatiblep a joba))
               (uplo (let ((c (aref (symbol-name uplo) 0)))
                       (if (char= opa joba) c (ecase c (#\U #\L) (#\L #\U)))))
               (alpha (t.coerce ,(field-type (cl :x)) alpha) :type ,(field-type (cl :x))))
         (declare (type base-char opa uplo))
         (if (tensor-vectorp b)
             (t.blas-trsv! ,(cl :x) uplo opa diag a lda (scal! alpha b) (strides b 0))
             (with-columnification (() (b))
               (t.blas-trsm! ,(cl :x) side uplo opa diag alpha a lda b (or (blas-matrix-compatiblep b) 0)))))))
  'B)

;; (let* ((A (tricopy! 0 (randn '(10 10)) :uo))
;;        (b (randn '(2 10)))
;;        (x (trs! 1 A (copy b) :nur :l)))
;;   #i(x - (/ tricopy! (1, A,\ :d) * b)))
