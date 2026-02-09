;;; coord.lisp --- Tensor Coordinate Accessor

;; 

;;; Code:
(in-package :obj/tensor)

;;Skip for now.
(declaim (ftype (function (coordinate-accessor &optional index-type) (or index-store-matrix index-store-vector)) indices))

(definline indices (x &optional idx)
  (declare (type coordinate-accessor x))
  (typecase idx
    (null (the index-store-matrix (slot-value x 'indices)))
    (index-type (lety* ((midx (slot-value x 'indices) :type index-store-matrix)
                        (order (array-dimension midx 1) :type index-type))
                  (vector-copy order midx (* idx order) (t.store-allocator index-store-vector order) 0 :key #'row-major-aref :lock #'(setf aref))))))

(definline coordinate-indexing (idx tensor)
  (declare (type index-store-vector idx) (type coordinate-accessor tensor))
  (lety* ((hash-value (stride-hash idx (strides tensor)) :type index-type)
          (hash-vector (slot-value tensor 'stride-hash) :type index-store-vector))
    (with-optimization (:speed 3 :safety 0) 
      (binary-search hash-value 0 (the index-type (slot-value tensor 'tail)) hash-vector))))

(define-tensor-method ref ((x coordinate-tensor :x) &rest subscripts)
  `(if-let ((idx (coordinate-indexing (%tensor-ref-subscripts subscripts x) x)))
     (values (t.store-ref ,(cl :x) (t.store ,(cl :x) x) (the index-type idx)) t)
     (values (t.fid+ (t.field-type ,(cl :x))) nil)))

(define-tensor-method (setf ref) (value (x coordinate-tensor :x) &rest subscripts)
  ;; TODO
  `(letv* ((subs/v (%tensor-ref-subscripts subscripts x) :type index-store-vector)
           (m lb (coordinate-indexing subs/v x)))
     (if m
         (values (setf (t.store-ref ,(cl :x) (t.store ,(cl :x) x) (the index-type m)) (t.coerce ,(field-type (cl :x)) value)) t)
         (if *sparse-tensor-realloc-on-setf*
             (with-memoization ()
               (memoizing lb :type index-type :bind lb)
               (memoizing (- (memoizing (total-size x) :type index-type) lb) :type index-type :bind r-len)
               (memoizing (order x) :type index-type)
               (if (< (length (memoizing (slot-value x 'stride-hash) :type index-store-vector :bind stride-hash)) (memoizing (store-size x)))
                   (progn ; very-quickly
                     (vector-copy (* r-len (memoizing (order x)))
                                  (memoizing (indices x) :type index-store-matrix) (* lb (memoizing (order x)))
                                  (memoizing (indices x)) (* (1+ lb) (memoizing (order x)))
                                  :key #'row-major-aref :lock #'(setf row-major-aref))
                     (vector-copy r-len stride-hash lb stride-hash (1+ lb) :key #'aref :lock #'(setf aref))
                     (vector-copy r-len
                                  (memoizing (t.store ,(cl :x) x) :type ,(store-type (cl :x))) lb
                                  (memoizing (t.store ,(cl :x) x)) (1+ lb)
                                  :key #'(lambda (a_ i_) (declare (index-type i_)) (t.store-ref ,(cl :x) a_ i_))
                                  :lock #'(lambda (v_ a_ i_) (declare (type index-type i_) (type ,(field-type (cl :x)) v_)) (t.store-set ,(cl :x) v_ a_ i_))))
                   (lety* ((ss (+ (memoizing (store-size x)) *default-sparse-store-increment*))
                           (idx-new (t.store-allocator index-store-matrix (list ss (memoizing (order x)))) :type index-store-matrix)
                           (hsh-new (t.store-allocator index-store-vector ss) :type index-store-vector)
                           (sto-new (t.store-allocator ,(cl :x) ss) :type ,(store-type (cl :x))))
                     (progn ;very-quickly
                       ;;Index
                       (vector-copy (* lb (memoizing (order x)))
                                    (memoizing (indices x)) 0 idx-new 0
                                    :key #'row-major-aref :lock #'(setf row-major-aref))
                       (vector-copy (* r-len (memoizing (order x)))
                                    (memoizing (indices x)) (* lb (memoizing (order x)))
                                    idx-new (* (1+ lb) (memoizing (order x)))
                                    :key #'row-major-aref :lock #'(setf row-major-aref))
                       ;;Hash
                       (vector-copy lb stride-hash 0 hsh-new 0 :key #'aref :lock #'(setf aref))
                       (vector-copy r-len stride-hash lb hsh-new (1+ lb) :key #'aref :lock #'(setf aref))
                       ;;Store
                       (vector-copy lb (memoizing (t.store ,(cl :x) x)) 0 sto-new 0
                                    :key #'(lambda (a_ i_) (declare (index-type i_)) (t.store-ref ,(cl :x) a_ i_))
                                    :lock #'(lambda (v_ a_ i_) (declare (type index-type i_) (type ,(field-type (cl :x)) v_)) (t.store-set ,(cl :x) v_ a_ i_)))
                       (vector-copy r-len (memoizing (t.store ,(cl :x) x)) lb  sto-new (1+ lb)
                                    :key #'(lambda (a_ i_) (declare (index-type i_)) (t.store-ref ,(cl :x) a_ i_))
                                    :lock #'(lambda (v_ a_ i_) (declare (type index-type i_) (type ,(field-type (cl :x)) v_)) (t.store-set ,(cl :x) v_ a_ i_))))
                     (setf (slot-value x 'indices) idx-new (slot-value x 'stride-hash) hsh-new (slot-value x 'store) sto-new)))
               (vector-copy (memoizing (order x)) subs/v 0 (indices x) (* lb (memoizing (order x))))
               (values (setf
                        (aref (slot-value x 'stride-hash) lb) (stride-hash subs/v (strides x))
                        (t.store-ref ,(cl :x) (t.store ,(cl :x) x) lb) (t.coerce ,(field-type (cl :x)) value))
                       nil))
             (error "missing entry in the sparse matrix ~a" subs/v)))))

;; (remmeth #'zeros-generic '(list (eql #.(tensor 'double-float 'coordinate-tensor))))

;; (let ((ret (zeros '(2 2) (tensor 'double-float 'coordinate-tensor) 4)))
;;   ret)

;; (let ((ret (zeros '(10 10) (tensor 'double-float 'coordinate-tensor) 4))) (indices ret))

;; (copy! (graph '((0) (1 0)) (tensor 'double-float 'graph-tensor)))
