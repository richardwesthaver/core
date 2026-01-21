;;; syl.lisp --- Sylvester Equation

;; 

;;; Code:
(in-package :math/lapack)

(define-template-generic (t.lapack-trsyl! #'subtypep) sym (op.A op.B sgn A ld.a B ld.b C ld.c))
(define-template-method t.lapack-trsyl! (sym blas-mixin) (op.A op.B sgn A ld.a B ld.b C ld.c)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (op.A op.B sgn A ld.a B ld.b C ld.c))
      `(let (,@decl)
         (declare (type character ,op.A ,op.B ,sgn)
                  (type ,sym ,A ,B ,C)
                  (type index-type ,ld.a ,ld.b ,ld.c))
         (with-alien ((ret1 int 0)
                      (ret2 int 0))
           (,(lapackfunc "trsyl" ftype)
            ,op.a ,op.b (if (char= ,sgn #\P) 1 -1)
            (dimensions ,C 0) (dimensions ,C 1)
            (deref (the ,(store-type sym) (store ,A)) (head ,A))
            ,ld.a
            (deref (the ,(store-type sym) (store ,B)) (head ,B))
            ,ld.b
            (deref (the ,(store-type sym) (store ,C)) (head ,C))
            ,ld.c
            #+nil
            (:& ,(lisp->mffi (field-type (realified-tensor sym))) :output) 
            (addr (t.fid* ,(field-type (realified-tensor sym))))
            (addr ret))
           ret)))))

(eval-always
  (defgeneric trsyl! (A B C &optional job)
    (:documentation "
    Syntax
    ------
    (trsyl! A B C &optional job)
    Computes the solution to
              op(A) X \pm X op(B) = C
    where A, B are (quasi) upper triangular matrices.

    The variable JOB can take on any of :{n, t/c}{n, t/c}{p, n}; the
    first two alphabets maps to op A, op B; whilst the third one controls
    the sign in the equation (oh dear! which year have we landed in.).
")
    (:method :before ((A tensor) (B tensor) (C tensor) &optional (job :nnp))	   
      (destructuring-bind (job.a job.b sgn) (split-job job)       
        (assert (and (typep A 'tensor-square-matrix) (typep B 'tensor-square-matrix)
                     (= (dimensions A (ecase job.a (#\N 0) ((#\C #\T) 1))) (dimensions C 0))
                     (= (dimensions B (ecase job.b (#\N 1) ((#\C #\T) 0))) (dimensions C 1))
                     (ziprm (or char=) (sgn sgn) (#\N #\P)))
                nil 'tensor-dimension-mismatch)))
    (:generic-function-class tensor-method-generator)))

(define-tensor-method trsyl! ((A blas-mixin :x) (B blas-mixin :x) (C blas-mixin :x t) &optional (job :nnp))
  `(destructuring-bind (op.a op.b sgn) (split-job job)
     (with-columnification (((A #\C) (B #\C)) (C))
       (letv* ((scale info (t.lapack-trsyl! ,(cl :x) op.a op.b sgn
                                            A (or (blas-matrix-compatiblep A #\N) 0)
                                            B (or (blas-matrix-compatiblep B #\N) 0)
                                            C (or (blas-matrix-compatiblep C #\N) 0))))
         (unless (= info 0)
           (if (< info 0)
               (error "TRSYL: Illegal value in the ~:r argument." (- info))
               (error "TRSYL: A and B have common or very close eigenvalues; perturbed values were used to solve the equation (but the matrices A and B are unchanged).")))
         (scal! scale C)))))

;;Should we use some fancy pattern matcher to do this ?
;;(solve #i(A' * ?x + ?x * B = C))
;;#i(a * b -> b * a, forall a in F, forall b in L(V, V))
;;#i(a * b \neq b * a, a, b are matrices)
;;#i(a + b -> b + a)
(defun syl (A B C &optional (job :nnp))
  "
    Syntax
    ------
    (syl A B C)
    Computes the solution to the Sylvester equation:
            op(A) X \pm X op(B) = C
    using Schur decomposition.

    The variable JOB can take on any of :{n, t/c}{n, t/c}{p, n}; the
    first two alphabets maps to op A, op B; whilst the third one controls
    the sign in the equation (oh dear! which year have we landed in.)."
  (letv* ((l.a t.a u.a (schur A) :type nil tensor tensor)
          (l.b t.b u.b (schur B) :type nil tensor tensor)
          (ucu (gem 1 u.a (gem 1 c u.b nil nil :nn) nil nil :cn)))
    (trsyl! t.a t.b ucu job)
    (gem 1 u.a (gem 1 ucu u.b nil nil :nc) nil nil)))
