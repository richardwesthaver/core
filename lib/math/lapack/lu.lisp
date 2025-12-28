;;; lu.lisp --- LU Factorization

;; 

;;; Code:
(in-package :math/lapack)

(deft/generic (t.lapack-getrf! #'subtypep) sym (A lda ipiv))
(deft/method t.lapack-getrf! (sym blas-mixin) (A lda ipiv)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (A lda ipiv))
      `(let* (,@decl)
         (declare (type ,sym ,A)
                  (type (simple-array ,(element-type-to-alien :int) (*)) ,ipiv)
                  (type index-type ,lda))
         (,(lapackfunc "getrf" ftype)
          (dimensions ,A 0) (dimensions ,A 1)
          #+nil
          (:* ,(alien-to-element-type ftype) :+ (head ,A)) 
          (the ,(store-type sym) (store ,A)) (:& :int) ,lda
          (the (simple-array ,(element-type-to-alien :int) (*)) ,ipiv) 
          ;; FIX 2025-12-26: 
          0)))))

;;
(eval-always
  (defgeneric getrf! (A)
    (:documentation
     "
  Syntax
  ======
  (GETRF! a)

  Purpose
  =======
  Given an NxM matrix A, compute its LU factorization using
  partial pivoting, row or column interchanges:

                A = P * L * U  (if A is row-major ordered)
                A = L * U * P' (if A is col-major ordered)

  where:

         P: permutation matrix
         L: lower triangular with unit diagonal elements
            (lower trapezoidal when N>M)
         U: upper triangular
            (upper trapezoidal when N<M)

  Return Values
  =============
  [1] The factors L and U from the factorization A = P*L*U  where the
      unit diagonal elements of L are not stored. (overwriting A)
  [2] IPIV
  [3] INFO = T: successful
             i:  U(i,i) is exactly zero.
")
    (:method :before ((A tensor)) (assert (tensor-matrixp A) nil 'tensor-dimension-mismatch))
    (:generic-function-class tensor-method-generator)))

(define-tensor-method getrf! ((A blas-mixin :x))
  `(let ((upiv (make-array (vector-min (the index-store-vector (dimensions A))) :element-type ',(element-type-to-alien :int))))
     (declare (type (simple-array ,(element-type-to-alien :int) (*)) upiv))
     (with-columnification (() (A))
       (let ((info (t.lapack-getrf! ,(cl :x) A (or (blas-matrix-compatiblep A #\N) 0) upiv)))
         (unless (= info 0)
           (if (< info 0)
               (error "GETRF: the ~a'th argument had an illegal value." (- info))
               (warn 'singular-matrix :message "GETRF: U(~a, ~:*~a) is exactly zero. The factorization has been completed, but the factor U is exactly singular, and division by zero will occur if it is used to solve a system of equations." :position info)))))
     (setf (gethash 'getrf (memos A)) upiv)
     (values A (with-no-init-checks (make-instance 'permutation-pivot-flip :store (pflip.f->l upiv) :size (dimensions A 0))))))

(deft/generic (t.lapack-getrs! #'subtypep) sym (A lda B ldb ipiv transp))
(deft/method t.lapack-getrs! (sym blas-mixin) (A lda B ldb ipiv transp)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (A lda B ldb ipiv transp))
      `(let* (,@decl)
         (declare (type ,sym ,A ,B)
                  (type (simple-array ,(element-type-to-alien :int) (*)) ,ipiv)
                  (type index-type ,lda ,ldb)
                  (type character ,transp))
         (,(lapackfunc "getrs" ftype)
           (:& :char) ,transp
           (:& :int) (dimensions ,A 0) (:& :int) (dimensions ,B 1)
           (:* ,(alien-to-element-type ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :int) ,lda
           (:* :int) (the (simple-array ,(element-type-to-alien :int) (*)) ,ipiv)
           (:* ,(alien-to-element-type ftype) :+ (head ,B)) (the ,(store-type sym) (store ,B)) (:& :int) ,ldb
           (:& :int :output) 0)))))

(eval-always
  (defgeneric getrs! (A B &optional job ipiv)
    (:documentation "Solve a system of linear equations

    A * X = B  or  A' * X = B

with a general N-by-N matrix A using the LU factorization computed
by GETRF. A and IPIV are the results from GETRF, TRANS specifies
the form of the system of equations:

         = 'N':  A * X = B  (No transpose)

         = 'T':  A'* X = B  (Transpose)

         = 'C':  A'* X = B  (Conjugate transpose)

Return Values
[1] The NxM matrix X. (overwriting B)
[4] INFO = T: successful
           i:  U(i,i) is exactly zero.  The LU factorization
               used in the computation has been completed,
               but the factor U is exactly singular.
               Solution could not be computed.")
    (:method :before ((A tensor) (B tensor) &optional (job :n) ipiv)
      (declare (type (or null permutation) ipiv) (ignore job))
      (assert (and (tensor-matrixp A) (<= (order B) 2)
                   (= (dimensions A 0) (dimensions A 1) (dimensions B 0))
                   (or (not ipiv) (<= (permutation-size ipiv) (dimensions A 0))))
              nil 'tensor-dimension-mismatch))
    (:generic-function-class tensor-method-generator)))

(define-tensor-method getrs! ((A blas-mixin :x) (B blas-mixin :x t) &optional (job :n) ipiv)
  `(if (tensor-vectorp b)
       (getrs! a (suptensor~ b 2) job ipiv)
       (let ((upiv (if ipiv
                       (pflip.l->f (store (tensor-copy ipiv 'permutation-pivot-flip)))
                       (or (gethash 'getrf (memos A)) (error "Cannot find permutation for the PLU factorisation of A."))))
             (cjob (aref (symbol-name job) 0)))
         (declare (type (simple-array (signed-byte 32) (*)) upiv))
         (with-columnification (((A #\C)) (B))
           (let ((info (t.lapack-getrs! ,(cl :x)
                                        A (or (blas-matrix-compatiblep A #\N) 0)
                                        B (or (blas-matrix-compatiblep B #\N) 0)
                                        upiv cjob)))
             (unless (= info 0)
               (error "getrs returned ~a. the ~:*~a'th argument had an illegal value." (- info)))))))
  'B)
;;
(deft/generic (t.lapack-getri! #'subtypep) sym (A lda ipiv))
(deft/method t.lapack-getri! (sym blas-mixin) (A lda ipiv)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (A lda ipiv) (lwork xxx))
      `(let* (,@decl)
         (declare (type ,sym ,A)
                  (type (simple-array ,(element-type-to-alien :int) (*)) ,ipiv)
                  (type index-type ,lda))
         (with-lapack-query ,sym (,xxx ,lwork)
           (with-alien ((info int 0))
             (,(lapackfunc "getri" ftype)
              (dimensions ,A 0)
              #+nil
              (:* ,(alien-to-element-type ftype) :+ (head ,A)) 
              (the ,(store-type sym) (store ,A)) 
              ,lda
              ,ipiv
              ,xxx 
              ,lwork
              (addr info))))))))

(eval-always
  (defgeneric getri! (A &optional perm)
    (:documentation
     "Compute the inverse of A using the LU factorization returned by GETRF!")
    (:method :before ((A tensor) &optional ipiv)
      (declare (type (or null permutation) ipiv))
      (assert (and (typep A 'tensor-square-matrix) (or (not ipiv) (<= (permutation-size ipiv) (dimensions A 0)))) nil 'tensor-dimension-mismatch))
    (:generic-function-class tensor-method-generator)))

(define-tensor-method getri! ((a blas-mixin :x) &optional ipiv)
  `(let ((upiv (if ipiv (pflip.l->f (store (tensor-copy ipiv 'permutation-action)))
                   (or (gethash 'getrf (memos A)) (error "Cannot find permutation for the PLU factorisation of A.")))))
     (declare (type (simple-array (signed-byte 32) (*)) upiv))
     (with-columnification (() (A))
       (let ((info (t.lapack-getri! ,(cl :x) A (or (blas-matrix-compatiblep A #\N) 0) upiv)))
         (unless (= info 0)
           (if (< info 0)
               (error "GETRI: the ~a'th argument had an illegal value." (- info))
               (error 'singular-matrix :message "GETRI: U(~a, ~:*~a) is exactly zero." :position info)))))
     A))
;;
(defun lu (a &optional (split-lu? t))
  "Compute the LU decomposition of A. This function is an interface to GETRF!

If SPLIT-LU? is T, then return (L, U, P), otherwise returns (LU, P)."
  (declare (type blas-mixin a))
  (multiple-value-bind (lu perm) (getrf! (tensor-copy a))
    (if (not split-lu?) (values lu perm)
        (let* ((min.d (vector-min (dimensions lu)))
               (l (tricopy! 1 (tricopy! lu (zeros (list (dimensions lu 0) min.d) (class-of a)) :l) :d))
               (u (tricopy! lu (zeros (list min.d (dimensions lu 1)) (class-of a)) :u)))
          (values l u perm)))))

;; (let* ((a (randn '(10 10)))
;;        (x (randn '(10 5)))
;;        (b (t* a x)))
;;   (values (norm (t- x (getrs! (getrf! (tensor-copy a)) (tensor-copy b))))
;; 	  (norm (t- x (t* (getri! (getrf! (tensor-copy a))) b)))))
