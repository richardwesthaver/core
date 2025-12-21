;;; gem.lisp --- General Matrix/Vector Multiplication

;; 

;;; Code:
(in-package :math/blas)

(deft/generic (t.blas-gemv! #'subtypep) sym (alpha A lda x st-x beta y st-y transp))
(deft/method t.blas-gemv! (sym blas-mixin) (alpha A lda x st-x beta y st-y transp)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (alpha A lda x st-x beta y st-y transp) (m n))
      `(let* (,@decl
              (,m (dimensions ,A 0))
              (,n (dimensions ,A 1)))
         (declare (type ,sym ,A ,x ,y)
                  (type ,(field-type sym) ,alpha ,beta)
                  (type index-type ,st-x ,st-y ,lda ,m ,n))
         (,(blas-func "gemv" ftype)
          ,transp
          ,m ,n
          ,alpha
          (deref (the ,(store-type sym) (store ,A)) (head ,A)) 
          ,lda
          (deref (the ,(store-type sym) (store ,x)) (head ,x))
          ,st-x
          ,beta
          (deref (the ,(store-type sym) (store ,y)) (head ,y))
          ,st-y)
         ,y))))

(deft/generic (t.blas-gemm! #'subtypep) sym (alpha A lda B ldb beta C ldc transa opa opb))
(deft/method t.blas-gemm! (sym blas-mixin) (alpha A lda B ldb beta C ldc transa opa opb)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (alpha A lda B ldb beta C ldc transa opa opb) (m n k))   
      `(let* (,@decl
              (,m (dimensions ,C 0)) (,n (dimensions ,C 1))
              (,k (dimensions ,A (ecase ,transa (#\N 1) ((#\T #\C) 0)))))
         (declare (type ,sym ,A ,B ,C)
                  (type ,(field-type sym) ,alpha ,beta)
                  (type index-type ,lda ,ldb ,ldc ,m ,n ,k)
                  (type character ,transa ,opa ,opb))
         (,(blas-func "gemm" ftype)
          ,opa ,opb
          ,m ,n ,k
          ,alpha
          (deref (the ,(store-type sym) (store ,A)) (head ,A))
          ,lda
          (deref (the ,(store-type sym) (store ,B)) (head ,B))
          ,ldb
          ,beta
          (deref (the ,(store-type sym) (store ,C)) (head ,C))
          ,ldc)
         ,C))))
;;
(deft/generic (t.gemv! #'subtypep) sym (alpha A x beta y transp))
(deft/method t.gemv! (sym dense-tensor) (alpha A x beta y transp)
  (using-gensyms (decl (alpha A x beta y transp))
   `(let (,@decl)
      (declare (type ,sym ,A ,x ,y)
               (type ,(field-type sym) ,alpha ,beta)
               (type character ,transp))
      (scal! ,beta ,y)
      ,@(when (field-realp (field-type sym))
              `((when (char= ,transp #\C) (setq ,transp #\T))))
      ;;These loops are optimized for column major matrices
      (ecase ,transp
       (#\N (einstein-sum ,sym (j i) (ref ,y i) (* ,alpha (ref ,A i j) (ref ,x j)) nil))
       (#\T (einstein-sum ,sym (i j) (ref ,y i) (* ,alpha (ref ,A j i) (ref ,x j)) nil))
       ,@(unless (field-realp (field-type sym))
                 `((#\C (einstein-sum ,sym (i j) (ref ,y i) (* ,alpha (t.fc ,(field-type sym) (ref ,A j i)) (ref ,x j)) nil)))))
      ,y)))

(deft/generic (t.gemm! #'subtypep) sym (alpha A B beta C transa transb))
(deft/method t.gemm! (sym dense-tensor) (alpha A B beta C transa transb)
  (using-gensyms (decl (alpha A B beta C transa transb))
   `(let (,@decl)
      (declare (type ,sym ,A ,B ,C)
               (type ,(field-type sym) ,alpha ,beta)
               (type character ,transa ,transb))
      (scal! ,beta ,C)
      ,@(when (field-realp (field-type sym))
              `((when (char= ,transa #\C) (setq ,transa #\T))
                (when (char= ,transb #\C) (setq ,transb #\T))))
      ;;These loops are optimized for column major matrices
      ,(labels ((transpose-ref (mat)
                  `(ref ,(cadr mat) ,@(reverse (cddr mat))))
                (conjugate-ref (mat)
                  `(t.fc ,(field-type sym) ,mat))
                (generate-mm-code (transa transb)
                  (destructuring-bind (A-ref B-ref) (mapcar #'(lambda (mat trans) (ecase trans
                                                                                    ((#\N #\T) mat)
                                                                                    ((#\C) (conjugate-ref mat))))
                                                            (mapcar #'(lambda (mat trans) (ecase trans
                                                                                            ((#\N) mat)
                                                                                            ((#\T #\C) (transpose-ref mat))))
                                                                    (list `(ref ,A i k) `(ref ,B k j)) (list transa transb))
                                                            (list transa transb))
                    (let ((loopo (let ((ta (member transa '(#\T #\C)))
                                       (tb (member transb '(#\T #\C))))
                                      (cond
                                        ((and (not ta) (not tb)) `(j k i))
                                        ((and (not ta) tb) `(k j i))
                                        (t`(i j k))))))
                      `(einstein-sum ,sym ,loopo (ref ,C i j) (* ,alpha ,A-ref ,B-ref) nil)))))
               `(ecase ,transa
                  ,@(loop :for ta :across (if (field-realp (field-type sym)) "NT" "NTC")
                       :collect `(,ta (ecase ,transb
                                        ,@(loop :for tb :across (if (field-realp (field-type sym)) "NT" "NTC")
                                             :collect `(,tb ,(generate-mm-code ta tb))))))))
      ,C)))

(eval-always
  (defgeneric gem! (alpha A B beta C &optional job)
    (:documentation
     "
  Syntax
  ======
  (GEM! alpha a b beta c [job])

  Purpose
  =======
  Performs the GEneral Matrix/Vector Multiplication given by
               --      -      -

            C <- alpha * op(A) * op(B) + beta * C

  and returns C.

  alpha,beta are scalars and A,B,C are matrices.
  op(A) means either A or A'.

  JOB must be a keyword with two of these alphabets
     N                 Identity
     T                 Transpose
     C                 Hermitian transpose {conjugate transpose}
")
    (:method :before (alpha (A tensor) (B tensor) beta (C tensor) &optional (job :nn))
      (assert (not (or (eq A C) (eq B C))) nil 'invalid-arguments :message "GEM!: C = {A or B} is not allowed.")
      (letv* (((joba &optional (jobb #\N)) (split-job job)))
        (assert (and
                 (tensor-matrixp A)
                 (= (order B) (order C)) (if (char= jobb #\N) (<= (order C) 2) (tensor-matrixp C))
                 (let ((loga (ecase joba (#\N 0) ((#\T #\C) 1)))
                       (logb (ecase jobb (#\N 0) ((#\T #\C) 1))))
                   (and (= (dimensions C 0) (dimensions A (logxor 0 loga)))
                        (= (dimensions A (logxor 1 loga)) (dimensions B (logxor 0 logb)))
                        (or (not (tensor-matrixp C)) (= (dimensions C 1) (dimensions B (logxor 1 logb)))))))
                nil 'tensor-dimension-mismatch)))
    (:generic-function-class tensor-method-generator)))

(defmethod gem! (alpha A (B dense-tensor) beta (C dense-tensor) &optional (job :n))
  (axpy! (* alpha A) B (scal! beta C)))

(define-tensor-method gem! (alpha (A dense-tensor :x) (B dense-tensor :x) beta (C dense-tensor :x t) &optional (job :n))
  `(letv* ((alpha (t.coerce ,(field-type (cl :x)) alpha))
           (beta (t.coerce ,(field-type (cl :x)) beta))
           ((joba &optional (jobb #\N)) (split-job job)))
     (declare (type ,(field-type (cl :x)) alpha beta)
              (type base-char joba jobb))
     (if (tensor-vectorp C)
         ,(recursive-append
           (when (subtypep (cl :x) 'blas-mixin)
             `(if (call-alien-p A (t.blas-lb ,(cl :x) 2))
                  (with-columnification (((A joba)) ())
                    (letv* ((lda opa (blas-matrix-compatiblep A joba)))
                      (t.blas-gemv! ,(cl :x) alpha A lda B (strides B 0) beta C (strides C 0) opa)))))
           `(t.gemv! ,(cl :x) alpha A B beta C joba))
         ,(recursive-append
           (when (subtypep (cl :x) 'blas-mixin)
             `(if (call-alien-p C (t.blas-lb ,(cl :x) 3))
                  (with-columnification (((a joba) (b jobb)) (c))
                    (letv* ((lda opa (blas-matrix-compatiblep a joba))
                            (ldb opb (blas-matrix-compatiblep b jobb)))
                      (t.blas-gemm! ,(cl :x) alpha A lda B ldb beta C (or (blas-matrix-compatiblep c #\N) 0) joba opa opb)))))
           `(t.gemm! ,(cl :x) alpha A B beta C joba jobb))))
  'C)

;;---------------------------------------------------------------;;
(defgeneric gem (alpha a b beta c &optional job)
  (:documentation
 "Performs the GEneral Matrix Multiplication given by

           alpha * op(A) * op(B) + beta * C

and returns the result in a new matrix.

alpha,beta are scalars and A,B,C are matrices.
op(A) means either A or A'.

JOB must be a keyword with two of these alphabets
   N                 Identity
   T                 Transpose
   C                 Hermitian conjugate"))

(defmethod gem (alpha (A dense-tensor) (B dense-tensor) beta (C dense-tensor) &optional (job :n))
  (gem! alpha A B beta (tensor-copy C) job))

(defmethod gem (alpha (A dense-tensor) (B dense-tensor) (beta (eql nil)) (C (eql nil)) &optional (job :n))
  (gem! alpha A B 0
        (letv* (((joba &optional (jobb #\N)) (split-job job)))	  
          (zeros (list* (dimensions A (ecase joba (#\N 0) ((#\C #\T) 1)))
                        (when (tensor-matrixp B) (list (dimensions B (ecase jobb (#\N 1) ((#\C #\T) 0))))))
                 (ziprm (cclass-max class-of) (A B))))
        job))

;; FIX 2025-12-18: 
#+nil
(labels ((gem-compiler (function alpha A B beta C job)
           (trivia:match* (a b)
             (((or (list (and op-a (or 'ctranspose 'ctranspose~ 'transpose 'transpose~)) code-a) code-a)
               (or (list (and op-b (or 'ctranspose 'ctranspose~ 'transpose 'transpose~)) code-b) code-b))
              (when (and (or op-a op-b) (keywordp job))
                (letv* (((job-a &optional (job-b #\N job-bp)) (split-job job))
                        (mjob-a (change-jobchar op-a job-a))
                        (mjob-b (change-jobchar op-b job-b))
                        (newjob (intern (coerce (list* (or mjob-a job-a) (or (if mjob-b (list mjob-b)) (if job-bp (list job-b)))) 'string) :keyword)))
                  (when (or mjob-a mjob-b)
                    (with-gensyms (ma mb)
                      `(let ((,ma ,(if mjob-a code-a a))
                             (,mb ,(if mjob-b code-b b)))
                         (,function ,alpha ,ma ,mb ,beta ,C ,newjob))))))))))
  (define-compiler-macro gem! (&whole form alpha A B beta C &optional (job :n))
    (or (apply #'gem-compiler (list (first form) alpha A B beta C job)) form))
  (define-compiler-macro gem (&whole form alpha A B beta C &optional (job :n))
    (or (apply #'gem-compiler (list (first form) alpha A B beta C job)) form)))

#+nil
(funcall (compiler-macro-function 'gem) '(gem! alpha (transpose A) (ctranspose~ B) beta (tensor-copy C) :nc) nil)
(defparameter *tensor-contraction-functable* (make-hash-table :test 'equal))
(eval-always
  (defgeneric gett! (alpha a b beta c)
    (:documentation "Returns the tensor contraction of A B: C <- beta * C + alpha A · B")
    (:method :before (alpha (a tensor) (b tensor) beta (c tensor))
      (declare (ignore alpha beta))
      (assert (and (= (dimensions a -1) (dimensions b 0))
                   (=  (+ (order a) (order b) -2) (order c))
                   (dotimes (i (1- (order a)) t) (unless (= (dimensions a i) (dimensions c i)) (return nil)))
                   (dotimes (i (1- (order b)) t) (unless (= (dimensions b (1+ i)) (dimensions c (+ (order a) i -1))) (return nil))))
              nil 'tensor-dimension-mismatch))
    (:generic-function-class tensor-method-generator)))

(define-tensor-method gett! (alpha (a dense-tensor :x) (b dense-tensor :x) beta (c dense-tensor :x t))
  `(let ((func (or (gethash (list (order a) (order b) ',(cl :x)) *tensor-contraction-functable*)
                   (let ((asyms (loop for i from 0 below (1- (order a)) collect (gensym (format nil "a_~a" i))))
                         (bsyms (loop for i from 1 below (order b) collect (gensym (format nil "b_~a" i))))
                         (sumsym (gensym "idx")))
                     (format t "Generating contraction for orders : (~a, ~a)." (order a) (order b))
                     (setf (gethash (list (order a) (order b) ',(cl :x)) *tensor-contraction-functable*)
                           (compile-and-eval `(lambda-einstein (alpha a b c) (,',(cl :x)  (ref c ,@asyms ,@bsyms) (* alpha (ref a ,@asyms ,sumsym) (ref b ,sumsym ,@bsyms)) nil)))))))
         (alpha (t.coerce ,(field-type (cl :x)) alpha))
         (beta (t.coerce ,(field-type (cl :x)) beta)))
     (unless (t.f= ,(field-type (cl :x)) beta (t.fid* ,(field-type (cl :x)))) (scal! beta c))
     (funcall func alpha a b c)
     c))

(eval-always
  (defgeneric gekr! (alpha a b beta c)
    (:documentation "Returns the kronecker product product of a and b. C <- beta * C + alpha A ⊗ B")
    (:generic-function-class tensor-method-generator)))

(define-tensor-method gekr! (alpha (a dense-tensor :x) (b dense-tensor :x) beta (c dense-tensor :x))
  `(let* ((ret (zeros (append (dimensions a t) (dimensions b t)) ',(cl :x)))
          (ret-a (subtensor~ ret (append (make-list (order a) :initial-element '(nil nil))
                                         (make-list (order b) :initial-element 0)))))
     (with-memoization ()
       (loop for idx being the index from 0 below (dimensions b) 
             with-iterator ((:stride ((of-b (strides b) (head b))
                                      (of-r (subseq (strides ret) (order a)) (head ret)))))
             do (setf (slot-value ret-a 'head) of-r)
             do (axpy! (t.store-ref ,(cl :x) (memoizing (store b) :type ,(store-type (cl :x))) of-b) a ret-a)))
     ret))
