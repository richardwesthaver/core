;;; stride.lisp --- Tensor Stride Accessor

;; 

;;; Code:
(in-package :obj/tensor)

(declaim (ftype (function ((or stride-accessor coordinate-accessor) &optional index-type) (or index-type index-store-vector)) strides))

(definline strides (x &optional idx)
  (declare (type (or stride-accessor coordinate-accessor) x))
  (typecase idx
    (index-type (the index-type (aref (the index-store-vector (slot-value x 'strides)) (modproj (or idx 0) (order x) nil 0))))
    (null (the index-store-vector (slot-value x 'strides)))))

(defmethod head ((x stride-accessor))
  (slot-value x 'head))

(definline subscripts-check (subs dimensions)
  (declare (type index-store-vector dimensions))
  (macrolet ((check (vectorp)
               `(let* ((idx (t.store-allocator index-store-vector (length dimensions))))
                  ,@(if vectorp `((declare (type index-store-vector subs))))
                  (loop :for ii :of-type index-type :from 0 :below (length idx)
                     :for si :of-type index-type ,@(if vectorp `(:across) `(:in)) subs
                     :do (setf (aref idx ii) (modproj si (aref dimensions ii) nil nil))
                     :finally (progn (assert (= ii (length idx)) nil 'tensor-index-rank-mismatch) (return idx))))))
    (etypecase subs
      (list (check nil))
      (index-store-vector (check t)))))

(definline stride-hash (idx strides)
  "
  Syntax
  ======
  (STRIDE-HASH IDX STRIDES)

  Purpose
  =======
  Computes the sum:

    length(STRIDES)
       __
  HD + \  STRIDE  * IDX
       /_        i      i
     i = 0
  "

  (declare (type index-store-vector idx strides))
  (loop :for cidx :of-type index-type :across idx
     :for sidx :of-type index-type :across strides
     :summing (the index-type (* cidx sidx)) :of-type index-type))
;;
(definline invert-hash (hash sort-index strides dimensions)
  "uniqueness seems to make this O(n); ignoring rigour for now."
  (declare (type index-store-vector dimensions strides sort-index)
           (type index-type hash))
  (lety* ((idx (t.store-allocator index-store-vector (length dimensions))))
    (loop for ii across sort-index
          do (lety* ((p/i (signum ii)) (ii (abs ii))
                     (si (aref strides ii)) (di (aref dimensions ii))
                     (q (max 0 (min (+ (floor hash si) (if (< p/i 0) 1 0)) di)) :type index-type))
               (setf (aref idx ii) q)
               (decf hash (* q (aref strides ii))))
          finally (return (if (= hash 0) idx)))))

;;TODO: add a check.
(definline stride-pivot (strides)
  (declare (type index-store-vector strides))
  (letv* ((_ index (sort-index (copy-seq strides) #'> :key #'abs) :type nil index-store-vector)
          (sindex (copy-seq index)))
    (loop for idx across index
          for i from (length index) downto 0
          do (setf (aref sindex i) (* idx (if (or (= i (length index)) (= (signum (aref strides idx)) (signum (aref strides (aref index (1+ i)))))) 1 -1))))
    sindex))

;;Stride makers.
(macrolet ((defstride (fname col?)
             `(definline ,fname (dims)
                (declare (type index-store-vector dims))
                (lety ((stds (t.store-allocator index-store-vector (length dims)) :type index-store-vector))
                  (locally (declare (optimize (speed 3) (safety 0)))
                    (loop
                      ,@(if col?
                            `(for i of-type index-type from 0 below (length dims))
                            `(for i of-type index-type from (1- (length dims)) downto 0))
                      with st = 1
                      do (lety ((d (aref dims i) :type index-type))
                           (assert (> d 0) nil 'tensor-invalid-dimension-value :argument i :dimension d)
                           (setf (aref stds i) (the index-type st)
                                 st (the index-type (* st d))))
                      finally (return (values stds st))))))))
  (defstride make-stride-cmj t)
  (defstride make-stride-rmj nil)
  (definline make-stride (dims)
    (ecase *default-stride-ordering* (:row-major (make-stride-rmj dims)) (:col-major (make-stride-cmj dims)))))
;;
(defmethod initialize-instance :after ((tensor stride-accessor) &rest initargs)
  (declare (ignore initargs))
  (when *tensor-safety-p*
    (lety ((dims (dimensions tensor) :type index-store-vector)
                (linearp (vectorp (slot-value tensor 'store))))
      (assert (>= (head tensor) 0) nil 'tensor-invalid-head-value :head (head tensor) :tensor tensor)
      (if (not (slot-boundp tensor 'strides))
          (letv* ((stds size (make-stride dims) :type index-store-vector index-type))
            (setf (slot-value tensor 'strides) stds)
            (when linearp
              (assert (<= (+ (head tensor) size) (store-size tensor)) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx (+ (head tensor) (1- (total-size tensor))) :tensor tensor)))
          (locally (declare (optimize (speed 3) (safety 0)))
            (lety ((stds (strides tensor) :type index-store-vector))
              (loop :for i :of-type index-type :from 0 :below (order tensor)
                    :for sz :of-type index-type := (aref dims 0) :then (the index-type (* sz (aref dims i)))
                    :summing (the index-type (the index-type (* (aref stds i) (1- (aref dims i))))) :into lidx :of-type index-type
                    :do (assert (> (aref dims i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (aref dims i) :tensor tensor)
                    :finally (when linearp
                               (assert (>= (the index-type (store-size tensor)) (the index-type (+ (the index-type (head tensor)) lidx)) 0) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx (the index-type (+ (the index-type (head tensor)) lidx)) :tensor tensor)))))))))

(define-tensor-method ref ((x stride-accessor :x) &rest subscripts)
  `(lety ((off (+ (head x) (stride-hash 
                            (if (and (listp subscripts) (typep (car subscripts) 'index-store-vector) (= (length subscripts) 2))
                                (subscripts-check (the index-store-vector (car subscripts)) (dimensions x))
                                (subscripts-check (the list subscripts) (dimensions x)))
                            (strides x))) 
               :type index-type))
     (t.store-ref ,(cl :x) (t.store ,(cl :x) x) off)))

(define-tensor-method (setf ref) (value (x stride-accessor :x) &rest subscripts)
  `(lety ((off (+ (head x) (stride-hash 
                            (if (and (listp subscripts) (typep (car subscripts) 'index-store-vector) (= (length subscripts) 2))
                                (subscripts-check (the index-store-vector (car subscripts)) (dimensions x))
                                (subscripts-check (the list subscripts) (dimensions x)))
                            (strides x)))
               :type index-type))
     (t.store-set ,(cl :x) (t.coerce ,(field-type (cl :x)) value) (t.store ,(cl :x) x) off)))
