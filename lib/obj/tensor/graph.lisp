;;; graph.lisp --- Tensor Graph Accessor

;; 

;;; Code:
(in-package :obj/tensor)

(definline fence (g &optional idx)
  (declare (type graph-accessor g))
  (typecase idx
    (null (the index-store-vector (slot-value g 'fence)))
    (index-type (lety*((f (slot-value g 'fence) :type index-store-vector)
                       (idx (modproj (or idx 0) (1- (length f)) nil 0) :type index-type))
                  (values (aref f idx) (aref f (1+ idx)))))
    (t (vector-to-list (the index-store-vector (slot-value g 'fence))))))

(definline δ-I (g &optional i j)
  (declare (type graph-accessor g))
  (cart-etypecase (i j)
    ((null null) (the index-store-vector (slot-value g 'neighbours)))
    ((index-type boolean)
     (if j
         (letv* ((l r (fence g i))
                 (nn (slot-value g 'neighbours) :type index-store-vector))
           (loop :for ii :from l :below r :collect (aref nn ii)))
         (aref (the index-store-vector (slot-value g 'neighbours)) i)))
    ((index-type (eql :size)) (letv* ((l r (fence g i))) (- r l)))
    ((index-type index-type)
     (letv* ((l r (fence g i))
             (nn (slot-value g 'neighbours) :type index-store-vector))
       (locally (declare (optimize (speed 3) (safety 0))) (binary-search j l r nn))))))
;;
(definline graph-indexing! (idx tensor)
  (declare (type index-store-vector idx) (type graph-accessor tensor))
  (when (slot-value tensor 'transposep) (rotatef (aref idx 0) (aref idx 1)))
  (letv* ((l r (fence tensor (aref idx 1)) :type index-type index-type))
    (locally (declare (optimize (speed 3) (safety 0))) (binary-search (aref idx 0) l r (δ-i tensor)))))

(define-tensor-method ref ((x graph-accessor :x) &rest subscripts)
  `(if-let ((idx (graph-indexing! (match subscripts
                                     ((list* (and subs/v (type index-store-vector)) _) (subscripts-check (the index-store-vector subs/v) (dimensions x)))
                                     (_ (subscripts-check (the list subscripts) (dimensions x))))
                                   x)))
     (values (t.store-ref ,(cl :x) (t.store ,(cl :x) x) (the index-type idx)) t)
     (values (t.fid+ (t.field-type ,(cl :x))) nil)))

(define-tensor-method (setf ref) (value (x graph-accessor :x) &rest subscripts)
  `(letv* ((sub/v (match subscripts
                    ((list* (and subs/v (type index-store-vector)) _) (subscripts-check (the index-store-vector subs/v) (dimensions x)))
                    (_ (subscripts-check (the list subscripts) (dimensions x)))) :type index-store-vector)
           (m lb (graph-indexing! sub/v x)))
     (if m
         (values (setf (t.store-ref ,(cl :x) (t.store ,(cl :x) x) (the index-type m)) (t.coerce ,(field-type (cl :x)) value)) t)
         (if *sparse-tensor-realloc-on-setf*
             (with-memoization ()
               (memoizing lb :type index-type :bind lb)
               (memoizing (- (aref (memoizing (fence x) :type index-store-vector) (1- (length (memoizing (fence x))))) lb) :type index-type :bind r-len)
               (if (< (aref (memoizing (fence x)) (1- (length (memoizing (fence x))))) (length (memoizing (δ-i x) :type index-store-vector)))
                   (locally (declare (optimize (speed 3) (safety 0)))
                     (lvec-copy r-len (memoizing (δ-i x)) lb (memoizing (δ-i x)) (+ lb 1) :key #'aref :lock #'(setf aref))
                     (lvec-copy r-len (memoizing (t.store ,(cl :x) x) :type ,(store-type (cl :x))) lb (memoizing (t.store ,(cl :x) x)) (+ lb 1)
                                :key #'(lambda (a_ i_) (declare (index-type i_)) (t.store-ref ,(cl :x) a_ i_))
                                :lock #'(lambda (v_ a_ i_) (declare (type index-type i_) (type ,(field-type (cl :x)) v_)) (t.store-set ,(cl :x) v_ a_ i_))))
                   (let*-typed ((ss (+ (store-size x) *default-sparse-store-increment*))
                                (δ-new (t.store-allocator index-store-vector ss) :type index-store-vector)
                                (sto-new (t.store-allocator ,(cl :x) ss) :type ,(store-type (cl :x))))
                     (locally (declare (optimize (speed 3) (safety 0)))
                       (lvec-copy lb (memoizing (δ-i x)) 0 δ-new 0 :key #'aref :lock #'(setf aref))
                       (lvec-copy r-len (memoizing (δ-i x)) lb δ-new (+ lb 1) :key #'aref :lock #'(setf aref))
                       (lvec-copy lb (memoizing (t.store ,(cl :x) x)) 0 sto-new 0
                                  :key #'(lambda (a_ i_) (declare (index-type i_)) (t.store-ref ,(cl :x) a_ i_))
                                  :lock #'(lambda (v_ a_ i_) (declare (type index-type i_) (type ,(field-type (cl :x)) v_)) (t.store-set ,(cl :x) v_ a_ i_)))
                       (lvec-copy r-len (memoizing (t.store ,(cl :x) x)) lb sto-new (+ lb 1)
                                  :key #'(lambda (a_ i_) (declare (index-type i_)) (t.store-ref ,(cl :x) a_ i_))
                                  :lock #'(lambda (v_ a_ i_) (declare (type index-type i_) (type ,(field-type (cl :x)) v_)) (t.store-set ,(cl :x) v_ a_ i_))))
                     (setf (slot-value x 'neighbours) δ-new (slot-value x 'store) sto-new)))
               (loop :for i :from (1+ (aref sub/v 1)) :below (length (memoizing (fence x))) :do (incf (aref (memoizing (fence x)) i)))
               (values
                (setf (aref (δ-i x) (the index-type lb)) (aref sub/v 0)
                      (t.store-ref ,(cl :x) (t.store ,(cl :x) x) (the index-type lb)) (t.coerce ,(field-type (cl :x)) value))
                nil))
             (error "missing entry in the sparse matrix ~a" sub/v)))))

(define-tensor-method (setf ref) (value (x graph-accessor :x) &rest subscripts)
  `(letv* (((r c) subscripts :type (index-type index-type))
           (idx lb (graph-indexing subscripts x)))
     (when (slot-value x 'transposep) (rotatef r c))
     (unless idx
       (letv* ((δg (δ-i x) :type index-store-vector)
               (sto (t.store ,(cl :x) x) :type ,(store-type (cl :x))))
         (declare (type index-type lb))
         ,@(flet ((code (sfr sto δfr δto)
                        `(locally (declare (optimize (speed 3) (safety 0)))
                           ,@(unless (and (eql δfr δto) (eql sto sfr))
                                     `((loop :for i :of-type index-type :from 0 :below lb
                                          :do (setf (aref ,δto i) (aref ,δfr i)
                                                    (t.store-ref ,(cl :x) ,sto i) (t.store-ref ,(cl :x) ,sfr i)))))
                           (loop :for i :from lb :below (nth-value 1 (fence x -1))
                              :do (setf (aref ,δto (1+ i)) (aref ,δfr i)
                                        (t.store-ref ,(cl :x) ,sto (1+ i)) (t.store-ref ,(cl :x) ,sfr i))))))
                 `((if (> (store-size x) (nth-value 1 (fence x -1)))
                       ,(code 'sto 'sto 'δg 'δg)
                       (let*-typed ((ss (+ (store-size x) *default-sparse-store-increment*))
                                    (δ-new (t.store-allocator index-store-vector ss) :type index-store-vector)
                                    (sto-new (t.store-allocator ,(cl :x) ss) :type ,(store-type (cl :x))))
                         ,(code 'sto 'sto-new 'δg 'δ-new)
                         (setf (slot-value x 'neighbours) δ-new
                               (slot-value x 'store) sto-new)))))
         (let-typed ((f (fence x) :type index-store-vector))
           (loop :for i :from (1+ c) :below (length (fence x)) :do (incf (aref f i))))
         (setf idx lb)))
     (setf
      (aref (δ-i x) (the index-type idx)) r
      (t.store-ref ,(cl :x) (t.store ,(cl :x) x) (the index-type idx)) (t.coerce ,(field-type (cl :x)) value))))

(defmethod store-size ((obj graph-accessor)) (length (slot-value obj 'neighbours)))
;;
