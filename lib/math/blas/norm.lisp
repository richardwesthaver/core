;;; norm.lisp --- Normalization

;; 

;;; Code:
(in-package :math/blas)

(define-tensor-generic norm (vec &optional n))

(define-tensor-method norm ((vec dense-tensor :x) &optional (n 2))
  (let ((rtype (field-type (realified-tensor (cl :x)))))
    ;; TODO 2025-12-08: 
    `(cond
       ;;Element-wise
       ((and (typep n cl:real) (<= 1 n))
        (lety ((sum (t.fid+ ,rtype) :type ,rtype))
          (dorefs (idx (dimensions vec))
              ((ref vec :type ,(cl :x)))
              (setf sum (t.f+ ,rtype sum (expt (abs ref) n))))
          (expt sum (/ n))))
       ((eql n :sup)
        (tensor-foldl ,(cl :x) max vec (t.fid+ ,rtype) :init-type ,rtype :key cl:abs))
       ((listp n)
        (or
         (destructuring-case n
           ;;L-ijk...       
           ((:L p args)
            (assert (or (eql p :sup (and (typep p real) (<= 1 p)))))
            (if args
                (let ((nrm (zeros (subseq (dimensions vec) 1) ',(realified-tensor (cl :x))))
                      (sl (subtensor~ vec (list* '(nil nil) (make-list (1- (order vec)) :initial-element 0)))))
                  (with-memoization ()
                    (loop for idx being the index from 0 below (dimensions nrm) 
                          with-iterator ((:stride ((of-nrm (strides nrm) (head nrm))
                                                   (of-sl (subseq (strides vec) 1) (head sl)))))
                          do (setf
                              (slot-value sl 'head) of-sl
                              (t.store-ref ,(realified-tensor (cl :x)) (memoizing (store nrm) :type ,(store-type (realified-tensor (cl :x)))) of-nrm) (norm sl p))))
                  (norm nrm (list* :L args)))
                (norm vec p)))
           ;;Schatten
           ((:schatten p vec)
            (assert (and (typep p real) (<= 1 p) (typep vec 'tensor-matrix)))
            (norm (svd vec :nn) p)))
         (destructuring-bind (x vec)
             (assert (typep vec 'tensor-matrix))
           (cond
             ((equalp '(:schatten :sup) x)
              (ref (svd vec :nn) 0))
          ;;Operator
          ((and (eql (car x) :operator) (= (length x) 2))
           (assert (typep vec 'tensor-matrix))
           (ecase (second x)
             (1 (norm vec '(:L 1 :sup)))
             (2 (norm vec '(:schatten :sup)))
             (:sup (norm (transpose~ vec) '(:operator 1))))))))))))

(define-tensor-generic tensor-max (object &optional key))

(define-tensor-method tensor-max ((vec dense-tensor :x) &optional key)
  `(if key
       (let* ((ridx (make-list (order vec) :initial-element 0))
              (rval (funcall key (ref vec (coerce ridx 'index-store-vector)))))
         (dorefs (idx (dimensions vec))
           ((ref vec :type ,(cl :x)))
           (let ((kval (funcall key ref)))
             (when (> kval rval)
               (setf rval kval)
               (copy-vector-to-list idx ridx))))
         (values rval ridx))
       (lety* ((ridx (make-list (order vec) :initial-element 0))
                    (rval (apply #'ref (list* vec ridx)) :type ,(field-type (cl :x))))
         (dorefs (idx (dimensions vec))
                 ((ref vec :type ,(cl :x)))
           (lety ((r ref :type ,(field-type (cl :x))))
             (when (> r rval)
               (setf rval r)
               (copy-vector-to-list idx ridx))))
         (values rval ridx))))

(define-tensor-generic tensor-min (vec &optional key))

(define-tensor-method tensor-min ((vec dense-tensor :x) &optional key)
    `(if key
       (let* ((ridx (make-list (order vec) :initial-element 0))
              (rval (funcall key (ref vec (coerce ridx 'index-store-vector)))))
         (dorefs (idx (dimensions vec))
                 ((ref vec :type ,(cl :x)))
           (let ((kval (funcall key ref)))
             (when (< kval rval)
               (setf rval kval)
               (copy-vector-to-list idx ridx))))
         (values rval ridx))
       (lety* ((ridx (make-list (order vec) :initial-element 0))
                    (rval (apply #'ref (list* vec ridx)) :type ,(field-type (cl :x))))
         (dorefs (idx (dimensions vec))
                 ((ref vec :type ,(cl :x)))
           (lety ((r ref :type ,(field-type (cl :x))))
             (when (< r rval)
               (setf rval r)
               (copy-vector-to-list idx ridx))))
         (values rval ridx))))

(defun tr (mat)
  (tensor-sum (tricopy! mat (zeros (vector-min (dimensions mat)) (class-of mat)) :d)))

(definline normalize! (x &optional (n 1))
  (let ((norm (norm x n)))
    (values (scal! (/ norm) x) norm)))
