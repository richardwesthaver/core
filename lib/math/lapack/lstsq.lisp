;;; lstsq.lisp --- Least Squares

;; 

;;; Code:
(in-package :math/lapack)

(deft/generic (t.lapack-gelsy! #'subtypep) sym (A lda B ldb rcond))
(deft/method t.lapack-gelsy! (sym blas-mixin) (A lda B ldb rcond)
  (let* ((ftype (field-type sym)) (complex? (subtypep ftype 'cl:complex)))
    (using-gensyms (decl (A lda B ldb rcond) (lwork xxx xxr jpvt))
      `(let* (,@decl
                 (,jpvt (make-array (dimensions ,A 1) :element-type ',(alien-to-element-type 'int) :initial-element 0)))
         (declare (type ,sym ,A ,B)
                  (type index-type ,lda ,ldb)
                  (type ,(field-type (realified-tensor sym)) ,rcond)
                  (type (simple-array ,(alien-to-element-type 'int) (*)) ,jpvt))
         (with-field-elements ,sym (,@(when complex? `((,xxr (t.fid+ ,ftype) (dimensions ,A 1)))))
           (with-lapack-query ,sym (,xxx ,lwork)
             (with-alien ((ret1 int 0)
                          (ret2 int 0))
               (,(lapackfunc "gelsy" ftype)
                (:& :int) (dimensions ,A 0) (:& :int) (dimensions ,A 1) (:& :int) (dimensions ,B 1)
                #+nil
                (:* ,(lisp->mffi ftype) :+ (head ,A)) 
                (the ,(store-type sym) (store ,A)) 
                ,lda
                #+nil
                (:* ,(lisp->mffi ftype) :+ (head ,B)) 
                (the ,(store-type sym) (store ,B)) 
                ,ldb
                (the (simple-array ,(alien-to-element-type 'int) (*)) ,jpvt) 
                ,rcond 
                (addr ret1)
                ,xxx ,lwork
                ,@(when complex? `(,xxr))
                (addr ret2))
               (values ret1 ret2))))))))
;;
(defgeneric gelsy (A B &optional rcond)
  (:documentation "
   Syntax
   =======

   (GELSY A B &optional TOL)

   INPUT
   -----
   A       A Matlisp matrix of size M x N
   B       A Matlisp matrix of size M x P
   RCOND   A condition number

   OUTPUT
   ------
   X       A Matlisp matrix of size N x NRHS
   RANK    An integer

   Purpose
   =======

   Compute the minimum-norm solution to a real linear least
   squares problem:
       minimize || A * X - B ||
   using a complete orthogonal factorization of A.  A is an M-by-N
   matrix which may be rank-deficient.

   Several right hand side vectors b and solution vectors x can be
   handled in a single call; they are stored as the columns of the
   M-by-NRHS right hand side matrix B and the N-by-NRHS solution
   matrix X.

   The routine first computes a QR factorization with column pivoting:
       A * P = Q * [ R11 R12 ]
                   [  0  R22 ]
   with R11 defined as the largest leading submatrix whose estimated
   condition number is less than 1/RCOND.  The order of R11, RANK,
   is the effective rank of A.

   Then, R22 is considered to be negligible, and R12 is annihilated
   by orthogonal transformations from the right, arriving at the
   complete orthogonal factorization:
      A * P = Q * [ T11 0 ] * Z
                  [  0  0 ]
   The minimum-norm solution is then
      X = P * Z' [ inv(T11)*Q1'*B ]
                 [        0       ]
   where Q1 consists of the first RANK columns of Q.

   This routine is basically identical to the original xGELSX except
   three differences:
     o The call to the subroutine xGEQPF has been substituted by the
       the call to the subroutine xGEQP3. This subroutine is a Blas-3
       version of the QR factorization with column pivoting.
     o Matrix B (the right hand side) is updated with Blas-3.
     o The permutation of matrix B (the right hand side) is faster and
       more simple.

   Further Details
   ===============

   Based on contributions by
     A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
     E. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
     G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain

   =====================================================================
")
  (:method :before ((A tensor) (B tensor) &optional rcond)
     (assert (and (tensor-matrixp A) (<= (order B) 2) (= (dimensions A 0) (dimensions B 0))) nil 'tensor-dimension-mismatch)
     (assert (or (null rcond) (> rcond 0)) nil 'invalid-value :expected '(> rcond 0) :given rcond :message "Invalid rcond."))
  (:generic-function-class tensor-method-generator))

(define-tensor-method gelsy ((A blas-mixin :x) (B blas-mixin :x t) &optional rcond)
  `(if (tensor-vectorp B)
       (orphanize (slice~ (gelsy A (matrixify~ B) rcond) 1))
       (let* ((rcond (or rcond (* *rcond-scale* ,(ecase (or (real-subtypep (field-type (cl :x))) (field-type (cl :x)))
                                                        (single-float single-float-epsilon)
                                                        (double-float double-float-epsilon)))))
              (A (with-colm (copy A ',(cl :x)))))
         (declare (type ,(cl :x) A))
         (let* ((mn (vector-max (dimensions A)))
                (X (with-colm (zeros (list mn (dimensions B 1)) ',(cl :x)))))
           (copy! B (subtensor~ X `((0 ,(dimensions B 0)) (nil nil))))
           (letv* ((rank info (t.lapack-gelsy! ,(cl :x) A (or (blas-matrix-compatiblep A #\N) 0) X (or (blas-matrix-compatiblep X #\N) 0) rcond)))
             (unless (= info 0)
               (error "gelsy returned ~a." info))
             (values (copy (subtensor~ X `((0 ,(dimensions A 1)) (nil nil)))) rank))))))

(definline lstsq (A B &optional rcond)
  (gelsy A B rcond))
