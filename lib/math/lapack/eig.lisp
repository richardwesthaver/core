;;; eig.lisp --- Eigenvalues

;; 

;;; Code:
(in-package :math/lapack)

(define-template-generic (t.lapack-geev! #'subtypep) sym (A lda vl ldvl vr ldvr wr wi))
(define-template-method t.lapack-geev! (sym blas-mixin) (A lda vl ldvl vr ldvr wr wi)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (A lda vl ldvl vr ldvr wr wi) (lwork xxx))
      `(let (,@decl)
         (declare (type ,sym ,A)
                  (type index-type ,lda)
                  (type ,(store-type sym) ,wr ,wi))
         (with-lapack-query ,sym (,xxx ,lwork)
           (with-alien ((ret int 0))
             (,(lapackfunc "geev" ftype)
              ,@(let ((args 
                        `((if ,vl #\V #\N) (if ,vr #\V #\N)
                          (dimensions ,A 0)
                          (deref (the ,(store-type sym) (store ,A)) (head ,A))
                          ,lda
                          (the ,(store-type sym) ,wr) 
                          (the ,(store-type sym) ,wi)
                          (deref (if ,vl (the ,(store-type sym) (store ,vl)) (nqull-pointer)) (if ,vl (head ,vl) 0))
                          (if ,vl ,ldvl 1)
                          (deref (if ,vr (the ,(store-type sym) (store ,vr)) (null-pointer)) (if ,vr (head ,vr) 0))
                          (if ,vr ,ldvr 1)
                          ,xxx 
                          ,lwork
                          (addr ret))))
                  (if (subtypep ftype 'cl:complex)
                      (apply #'append (permute (pair args) (make-instance 'permutation-cycle :store (list (idxv 12 11 10 9 8 7 6)))))
                      args)))))))))
;;
(define-template-generic (t.lapack-heev! #'subtypep) sym (jobz uplo A lda w))
(define-template-method t.lapack-heev! (sym blas-mixin) (jobz uplo A lda w)
  (using-gensyms (decl (jobz A lda w uplo) (lwork xxx xxr))
    (let ((complex? (subtypep (field-type sym) 'complex))
          (ftype (field-type sym)))
      `(let (,@decl)
         (declare (type ,sym ,A)
                  (type character ,jobz ,uplo)
                  (type index-type ,lda)
                  (type ,(store-type (realified-tensor sym)) ,w))
         (with-field-elements ,(realified-tensor sym) (,@(when complex? `((,xxr (t.fid+ (t.field-type (t.realified-tensor ,sym))) (* 3 (dimensions ,A 0))))))
           (with-lapack-query ,sym (,xxx ,lwork)
             (with-alien ((ret int 0))
               (,(lapackfunc (if complex? "heev" "syev") ftype)
                ,jobz ,uplo
                (dimensions ,A 0)
                (deref (the ,(store-type sym) (store ,A)) (head ,A))
                ,lda
                ,w
                ,xxx ,lwork
                ,@(when complex? `(,xxr))
                (addr ret))
               ret)))))))

;;
(define-template-generic (t.geev-output-fix #'subtypep) sym (wr wi))
(define-template-method t.geev-output-fix (sym blas-mixin) (wr wi)
  (if (clinear-storep sym)
      (using-gensyms (decl (wr))
        `(let (,@decl)
           (declare (type ,(store-type sym) ,wr))
           ,wr))
      (let ((csym (complexified-tensor sym)))
        (using-gensyms (decl (wr wi) (ret i))
          `(let* (,@decl
                     (,ret (t.store-allocator ,csym (t.store-size ,sym ,wr))))
             (declare (type ,(store-type sym) ,wr ,wi)
                      (type ,(store-type csym) ,ret))
             (with-optimization (:speed 3 :safety 0)
               (loop :for ,i :from 0 :below (t.store-size ,sym ,wr)
                     :do (t.store-set ,csym (complex (t.store-ref ,sym ,wr ,i) (t.store-ref ,sym ,wi ,i)) ,ret ,i)))
             ,ret)))))

(eval-always
  (defgeneric geev! (a &optional vl vr)
    (:documentation "
 Syntax
 ======
 (GEEV! a &optional vl vr)

 Purpose:
 ========
 Computes the eigenvalues and left/right eigenvectors of A.

 For an NxN matrix A, its eigenvalues are denoted by:

              lambda(i),   j = 1 ,..., N

 The right eigenvectors of A are denoted by v(i) where:

                    A * v(i) = lambda(i) * v(i)

 The left eigenvectors of A are denoted by u(i) where:

                     H                      H
                 u(i) * A = lambda(i) * u(i)

 In matrix notation:
                             -1
                    A = V E V

           and
                          -1
                         H       H
                    A = U    E  U

 where lambda(i) is the ith diagonal of the diagonal matrix E,
 v(i) is the ith column of V and u(i) is the ith column of U.

 The computed eigenvectors are normalized to have Euclidean norm
 equal to 1 and largest component real.
 ")
    (:method :before ((a tensor) &optional vl vr)
      (assert (typep a 'tensor-square-matrix) nil 'tensor-dimension-mismatch)
      (when vl
        (assert (and (vector-eq (dimensions a) (dimensions vl)) (typep vl (type-of a)))  nil 'tensor-dimension-mismatch))
      (when vr
        (assert (and (vector-eq (dimensions a) (dimensions vr)) (typep vr (type-of a)))  nil 'tensor-dimension-mismatch)))
    (:generic-function-class tensor-method-generator)))

(define-tensor-method geev! ((a blas-mixin :x t) &optional vl vr)
  `(let* ((jobvl (if vl #\V #\N))
          (jobvr (if vr #\V #\N))
          (n (dimensions A 0))
          (wr (t.store-allocator ,(cl :x) n))
          (wi (t.store-allocator ,(cl :x) n)))
     (ecase jobvl
       ,@(loop :for jvl :in '(#\N #\V) :collect
                  `(,jvl
                    (ecase jobvr
                      ,@(loop :for jvr :in '(#\N #\V) :collect
                                 `(,jvr
                                   (with-columnification (() (A ,@(when (char= jvl #\V) `(vl)) ,@(when (char= jvr #\V) `(vr))))
                                     (let ((info (t.lapack-geev! ,(cl :x)
                                                                 A (or (blas-matrix-compatiblep A #\N) 0)
                                                                 ,@(if (char= jvl #\N) `(nil 1) `(vl (or (blas-matrix-compatiblep vl #\N) 0)))
                                                                 ,@(if (char= jvr #\N) `(nil 1) `(vr (or (blas-matrix-compatiblep vr #\N) 0)))
                                                                 wr wi)))
                                       (unless (= info 0)
                                         (if (< info 0)
                                             (error "GEEV: Illegal value in the ~:r argument." (- info))
                                             (error "GEEV: the QR algorithm failed to compute all the eigenvalues, and no eigenvectors have been computed;
elements ~a:~a of WR and WI contain eigenvalues which have converged." info n)))))))))))
     (let ((ret nil))
       (when vr (push vr ret))
       (when vl (push vl ret))
       (values-list (list* (with-no-init-checks
                               (make-instance ',(complexified-tensor (cl :x))
                                 :dimensions (coerce (list (dimensions A 0)) 'index-store-vector)
                                 :strides (coerce (list 1) 'index-store-vector)
                                 :head 0
                                 :store (t.geev-output-fix ,(cl :x) wr wi)))
                           ret)))))
(eval-always
  (defgeneric heev! (a &optional job uplo?)
    (:documentation "
 Syntax
 ======
 (HEEV! a &optional evec? )

 Purpose:
 ========
 Computes the eigenvalues / eigenvectors of a Hermitian (symmetric) A.
 ")
    (:method :before ((a tensor) &optional (job :n) (uplo? *default-uplo*))
      (assert (typep a 'tensor-square-matrix) nil 'tensor-dimension-mismatch)
      (assert (and (member job '(:v :n)) (member uplo? '(:u :l))) nil 'invalid-arguments))
    (:generic-function-class tensor-method-generator)))

(define-tensor-method heev! ((a blas-mixin :x) &optional (job :n) (uplo? *default-uplo*))
  `(let ((evals (zeros (dimensions a 0) ',(realified-tensor (cl :x)))))
     (with-columnification (() (A))
       (let ((info (t.lapack-heev! ,(cl :x)
                                   (aref (symbol-name job) 0)
                                   (aref (symbol-name uplo?) 0)
                                   A (or (blas-matrix-compatiblep A #\N) 0)
                                   (store evals))))
         (unless (= info 0)
           (if (< info 0)
               (error "(SY/HE)EV: Illegal value in the ~:r argument." (- info))
               (error "(SY/HE)EV: the algorithm failed to converge; ~a off-diagonal elements of an intermediate tridiagonal form did not converge to zero." info)))))
     (values-n (if (eq job :v) 2 1) evals A)))
;;
(definline geev-complexify-eigvec (eigval eigvec)
  (let* ((n (dimensions eigval 0))
         (evec (tensor-copy eigvec (complexified-tensor (class-of eigvec))))
         (tmp (zeros n (complexified-tensor (class-of eigvec))))
         (cviewa (slice~ evec 1 0)) (cviewb (slice~ evec 1 0))
         (cst (aref (strides evec) 1)))
    (loop for i = 0 with hd = (head cviewa)
          do (cond
               ((>= i n) (return nil))
               ((zerop (cl:imagpart (ref eigval i))) (incf i))
               (t (setf (slot-value cviewa 'head) (+ hd (* i cst))
                        (slot-value cviewb 'head) (+ hd (* (1+ i) cst)))
                  (copy! cviewb tmp) (copy! cviewa cviewb)
                  (axpy! #c(0 1) tmp cviewa) (axpy! #c(0 -1) tmp cviewb)
                  (incf i 2))))
    evec))

(defun eig (x &optional (job :nn) (uplo *default-uplo*))
  (declare (type (and tensor-square-matrix blas-mixin) x))
  (let ((*default-tensor-type* (class-of x)))
    (if (clinear-storep (class-of x))
        (ecase job
          ((:nn :nv :vn :vv)
           (letv* (((levec? revec?) (mapcar #'(lambda (x) (char= x #\V)) (split-job job))))
             (geev! (tensor-copy x) (when levec? (zeros (dimensions x))) (when revec? (zeros (dimensions x))))))
          ((:n :v)
           (heev! (tensor-copy x) job uplo)))
        (ecase job
          ((:nn :nv :vn :vv)
           (letv* (((levec? revec?) (mapcar #'(lambda (x) (char= x #\V)) (split-job job)))
                   (ret (multiple-value-list (geev! (tensor-copy x) (when levec? (zeros (dimensions x))) (when revec? (zeros (dimensions x)))))))
             (let ((eval (first ret)))
               (unless (dotimes (i (dimensions x 0) t) (unless (zerop (cl:imagpart (ref eval i))) (return nil)))
                 (setq ret (list* eval (mapcar #'(lambda (x) (geev-complexify-eigvec eval x)) (cdr ret))))))
             (values-list ret)))
          ((:n :v)
           (heev! (tensor-copy x) job uplo))))))

;;Must add tests.
;; #+nil
;; (let ((a  #i(randn([3, 3]) + 1i * randn ([3, 3]))))
;;   (octave-send-tensor a "a")
;;   (octave-send "[v, l] = eig(a);~%")
;;   (letv* ((s vl vr (eig a :vv))
;; 	  (m/s (slice~ (octave-read-tensor "diag(l)") -1))
;; 	  (m/vr (octave-read-tensor "v")))
;;     (mapcar #'norm (list #i(a - (/vl)' * diag(s) * vl') #i(a - vr * diag(s, 2) * /vr) #i(\m/s - s) #i(\m/vr - vr)))
;;     ;;(geev! a nil (zeros (dims a) (class-of a)))

;;     #+nil(norm #i(vr * diag(s, 2) * /vr - a))
;;     #+nil(norm (t- a (t* v (diag s 2) (inv v))))
;;     #+nil(values (norm (t- (diag~ (octave-read-tensor "l")) s))
;; 		 (norm (t- (octave-read-tensor "v") v)))))

;; originally from blas/norm.lisp
(defun psd-proj (m)
  (letv* ((λλ u (eig (scal! 1/2 (axpy! 1 (transpose~ m) (tensor-copy m))) :v))
          (ret (zeros (dimensions m) (type-of m))))
    (loop for (λi ui) being the slice of (list λλ u) along (list 0 -1)
          if (< 0 (ref λi 0)) do (ger! (ref λi 0) ui ui ret t))
    ret))
