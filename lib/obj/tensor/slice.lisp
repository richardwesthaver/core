;;; slice.lisp --- Tensor Slices

;; 

;;; Code:
(in-package :obj/tensor)

(defmethod subtensor~ :before ((tensor base-tensor) (subscripts list))
  (assert (or (null subscripts) (= (length subscripts) (order tensor))) nil 'tensor-index-rank-mismatch))

(defmethod (setf subtensor~) (value (tensor dense-tensor) (subscripts list))
  (letv* ((hd dims stds (parse-slice-for-strides subscripts (dimensions tensor) (strides tensor))))
    (cond
      ((not hd) nil #+nil(error "no place found inside ~a." subscripts))
      ((not dims) (if subscripts
                      (setf (store-ref tensor hd) value)
                      (copy! value (without-tensor-safety (subtensor~ tensor nil)))))
      (t (copy! value
                (without-tensor-safety
                    (make-instance (class-of tensor)
                      :head (+ hd (head tensor))
                      :dimensions (coerce dims 'index-store-vector)
                      :strides (coerce stds 'index-store-vector)
                      :store (slot-value tensor 'store)
                      :parent tensor)))))))

(definline parse-slice (subs dimensions)
  (declare (type index-store-vector dimensions))
  (let ((dims) (psubs))
    (loop for sub.i in subs
          for d of-type index-type across dimensions
          do (if (not (consp sub.i))
                 (let ((idx (modproj (the (or index-type null) sub.i) d nil 0)))
                   (push 1 dims)
                   (push idx psubs))
                 (destructuring-bind (start end . inc) sub.i
                   (declare ((or index-type null) start end inc))
                   (let* ((inc (modproj inc nil nil 1))
                          (start (modproj start d nil (if (> inc 0) 0 (1- d))))
                          (end (modproj end d t (if (> inc 0) d -1)))
                          (nd (ceiling (- end start) inc)))
                     (declare (type index-type start end inc nd))
                     (when (<= nd 0) (return nil))
                     (push nd dims)
                     (push (list* start end inc) psubs))))
          finally (return (values (nreverse psubs) (nreverse dims))))))

(definline parse-slice-for-strides (subscripts dimensions strides)
  (declare (type index-store-vector dimensions strides)
           (type list subscripts))
  (let ((dims) (stds))
    (loop for sub.i in subscripts
          for d across dimensions
          for s across strides
          with hd = 0
          if (not (consp sub.i))
          do (let ((idx (modproj (the (or index-type null) sub.i) d nil 0)))
               (incf hd (* s idx)))
          else do
             (destructuring-bind (start end . inc) sub.i
               (declare ((or index-type null) start end inc))
               (let* ((inc (modproj inc nil nil 1))
                      (start (modproj start d nil (if (> inc 0) 0 (1- d))))
                      (end (modproj end d t (if (> inc 0) d -1)))
                      (nd (ceiling (- end start) inc)))
                 (declare (type index-type start end inc nd))
                 (when (<= nd 0) (return nil))
                 (incf hd (* s start))
                 (push nd dims)
                 (push (* inc s) stds)))
          finally (return (values hd (nreverse dims) (nreverse stds))))))

(definline slice~ (x axis &optional (idx 0) (preserve-rank-p (when (= (order x) 1) t)))
  (let* ((axis (modproj axis (order x) nil 0))
         (subs (loop for i from 0 below (order x)
                     collect (cond ((/= i axis) '(nil nil))
                                   (preserve-rank-p (list idx (1+ idx)))
                                   (t idx)))))
    (subtensor~ x subs)))

(defgeneric suptensor (tensor ord &optional start)
  (:method :before ((tensor base-tensor) ord &optional (start 0))
    (declare (type index-type start))
    (assert (<= 0 start (- ord (order tensor))) nil 'invalid-arguments)))

(defmethod suptensor ((ten dense-tensor) ord &optional (start 0))
  (declare (type index-type ord start))
  (if (= (order ten) ord) ten
      (without-tensor-safety
          (make-instance (class-of ten)
            :dimensions (coerce (nconc (make-list start :initial-element 1)
                                       (vector-to-list (dimensions ten))
                                       (make-list (- ord (order ten) start) :initial-element 1))
                                'index-store-vector)
            :strides (coerce (nconc (make-list start :initial-element (total-size ten))
                                    (vector-to-list (strides ten))
                                    (make-list (- ord (order ten) start) :initial-element (total-size ten)))
                             'index-store-vector)
            :head (head ten) :store (slot-value ten 'store) :parent ten))))

(definline matrixify (vec &optional (col-vectorp t))
  (if (tensor-matrixp vec) vec (suptensor vec 2 (if col-vectorp 0 1))))

(defmethod reshape! ((ten dense-tensor) (dims cons))
  (let ((idim (coerce dims 'index-store-vector)))
    (setf (slot-value ten 'dimensions) idim
          (slot-value ten 'strides) (let ((strd (make-stride idim)))
                                      (when (< (strides ten 0) 0)
                                        (loop for i from 0 below (length strd)
                                              do (setf (aref strd i) (- (aref strd i)))))
                                      strd))
    ten))

(defun reshape (x dims) (reshape! (subtensor~ x nil) dims))

(defun join (axis tensor &rest more-tensors)
  (if (null tensor)
      (when more-tensors (apply #'join (list* axis more-tensors)))
      (let ((dims (copy-seq (dimensions tensor))))
        (loop for ele in more-tensors do (incf (aref dims axis) (aref (dimensions ele) axis)))
        (let* ((ret (zeros dims (class-of tensor)))
               (view (slice~ ret axis 0 t)))
          (loop for ele in (cons tensor more-tensors)
                with head = 0
                do (progn
                     (setf (slot-value view 'head) head
                           (aref (dimensions view) axis) (aref (dimensions ele) axis))
                     (copy! ele view)
                     (incf head (* (aref (strides ret) axis) (aref (dimensions ele) axis)))))
          ret))))
;;
(eval-always
  (defgeneric minors (x &rest indices)
    (:documentation "Copy minors of x corresponding to indices.")
    (:generic-function-class tensor-method-generator)))

(with-memoization ((make-hash-table :weakness :key-or-value :test 'equalp))
  (memoizing
   (defun minors-strides-precompute (dims std indices)
     (declare (type index-store-vector dims std))
     (macrolet ((kernel (jj) `(the index-type (* (aref std ii) (modproj (the index-type ,jj) (aref dims ii))))))
       (loop for idx in indices
             with stable = (make-array 0 :adjustable t)
             with ii of-type index-type = 0
             do (lety ((sv (t.store-allocator index-store-vector (length idx) :initial-element 0) :type index-store-vector))
                  (loop for i from 0 below (length sv)
                        for jj in idx 
                        do (locally (declare (type index-type i jj))
                             (setf (aref sv i) (kernel jj))))
                  (vector-push-extend sv stable))
             do (incf ii)
             finally (return stable))))))
