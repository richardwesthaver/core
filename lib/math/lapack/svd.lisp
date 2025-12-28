;;; svd.lisp --- Singular Value Decomposition

;; 

;;; Code:
(in-package :math/lapack)

(deft/generic (t.lapack-gesvd! #'subtypep) sym (A lda u ldu v ldv s))
(deft/method t.lapack-gesvd! (sym blas-mixin) (A lda u ldu v ldv s)
  (let* ((ftype (field-type sym)) (rtype (field-type (realified-tensor sym)))
         (complex? (subtypep ftype 'cl:complex)))
    (using-gensyms (decl (A lda u ldu v ldv s) (lwork xxx xxr))
      `(let (,@decl)
         (declare (type ,sym ,A)
                  (type ,(realified-tensor sym) ,s)
                  (type index-type ,lda))
         (with-field-elements ,(realified-tensor sym) (,@(when complex? `((,xxr (t.fid+ ,rtype) (* 5 (vector-min (dimensions ,A)))))))
           (with-lapack-query ,sym (,xxx ,lwork)
             (with-alien ((ret int 0))
               (,(lapackfunc "gesvd" ftype)
                (if ,u #\A #\N) (if ,v #\A #\N)
                (dimensions ,A 0) (dimensions ,A 1)
                #+nil
                (:* ,(lisp->mffi ftype) :+ (head ,A)) 
                (the ,(store-type sym) (store ,A)) 
                ,lda
                #+nil
                (:* ,(lisp->mffi rtype) :+ (head ,s)) 
                (the ,(store-type (realified-tensor sym)) (store ,s))
                #+nil
                (:* ,(lisp->mffi ftype) :+ (if ,u (head ,u) 0)) 
                (if ,u (the ,(store-type sym) (store ,u)) (null-pointer)) 
                (if ,u ,ldu 1)
                #+nil
                (:* ,(lisp->mffi ftype) :+ (if ,v (head ,v) 0)) 
                (if ,v (the ,(store-type sym) (store ,v)) (null-pointer)) 
                (if ,v ,ldv 1)
                (the ,(store-type sym) ,xxx) 
                ,lwork
                ,@(when complex? `(,xxr))
                (addr ret))
               ret)))))))
;;
(defgeneric svd (a &optional job)
  (:documentation
   "Compute the singular value decomposition (SVD) of the 
NxM matrix A. The SVD of A is given by:

               A = U * SIGMA * V'

where, taking p = min(n,m):

        U = [u1 u2 ... un] an NxN othogonal matrix

             [s1  0  0  ... 0]
    SIGMA =  [0  s2  0  ... 0]  if N < M
             [:   :  \\      :]
             [0   0  sp ... 0]

             [s1  0  0 ...  0]         
          =  [0  s2  0 ...  0]  if M > N
             [:   :  \\ ...  0]
             [:   :    \\    0]
             [0   0  0 ... sp]
             [0   0  0 ...  0]
             [:   :  :      :]
             [0   0  0 ...  0]

            [v1']
        V = [v2'] an MxM orthogonal matrix
            [ : ]
            [vm']

 The diagonal elements of SIGMA are the singular values of A.
 s1,...,sp are real, non-negative and arranged so that s1 >= s2 >= ... >= sp
 The first p columns of U are the left singular vectors of A.
 The first p rows of V' are the right singular vectors of A.

Return Values
=============

JOB              Return Value
-------------------------------------------------
:NN (default)   SIGMA                The p diagonal elements of SIGMA as a vector.
:UN             SIGMA, U
:NV             SIGMA, V
:UV             SIGMA, U, V")
  (:method :before ((a tensor) &optional (job :nn))
    (assert (member job '(:nn :un :nv :uv)) nil 'invalid-arguments))
  (:generic-function-class tensor-method-generator))

(define-tensor-method svd ((a blas-mixin :x) &optional (job :nn))
  `(destructuring-bind (ujob vjob) (split-job job)
     (let ((u (when (char= ujob #\U) (with-colm (zeros (list (dimensions a 0) (dimensions a 0)) ',(cl :x)))))
           (v (when (char= vjob #\V) (with-colm (zeros (list (dimensions a 1) (dimensions a 1)) ',(cl :x)))))
           (s (zeros (vector-min (dimensions a)) ',(realified-tensor (cl :x)))))
       (let ((info (t.lapack-gesvd! ,(cl :x) (with-colm (tensor-copy a)) (dimensions a 0) u (and u (dimensions u 0)) v (and v (dimensions v 0)) s)))
         (unless (= info 0)
           (if (< info 0)
               (error "GESVD: Illegal value in the ~:r argument." (- info))
               (error "GESVD: DBDSQR did not converge. ~a superdiagonals of an intermediate bidiagonal form B did not converge to zero. See the description of WORK in the LAPACK documentation." info))))
       (let ((ret nil))
         (when v (push (with-colm (transpose v)) ret))
         (when u (push u ret))
         (values-list (list* s ret))))))

;; (letv* ((a (randn '(10 10)))
;; 	(s u v (svd a :uv)))
;;   (norm (t- a (t* u (diag s 2) (transpose v)))))
