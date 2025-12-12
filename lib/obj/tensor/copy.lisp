;;; copy.lisp --- Tensor COPY

;; 

;;; Code:
(in-package :obj/tensor)

(define-tensor-generic copy! (from to)
  (:documentation
   "
  (COPY! x y)

  Copy the contents of X into Y. Return Y.
")
  ;; TODO 2025-12-08: 
  (:method :before ((x array) (y array))
    (assert (equal (array-dimensions x) (array-dimensions y)) nil 'dimension-mismatch))
  (:method  :before ((x array) (y tensor))
    (assert (equal (array-dimensions x) (dimensions y t)) nil 'dimension-mismatch))
  (:method :before ((x tensor) (y array))
    (assert (equal (array-dimensions y) (dimensions x t)) nil 'dimension-mismatch))  
  (:method :before ((x cons) (y cons))
    (assert (= (length x) (length y)) nil 'dimension-mismatch)))

(definline tensor-copy (obj &optional type)
  (copy! obj (etypecase type (symbol type) (standard-class (class-name type)))))

(defmethod copy! ((num number) (type symbol))
  (if type (coerce num type) num))

(defmethod copy! ((from cons) (to cons))
  (do ((flst from (cdr flst))
       (tlst to (cdr tlst)))
      ((or (null flst) (null tlst)))
    (cart-etypecase ((car flst) (car tlst))
      ((atom atom) (setf (car tlst) (car flst)))
      ((cons cons) (copy! (car flst) (car tlst)))))
  to)

(defmethod copy! ((lst cons) (type symbol))
  (labels ((list-dimensions (lst)
             (if (atom lst) nil
                 (cons (length lst) (list-dimensions (car lst))))))
    (cond
      ((member type '(list cons nil)) (copy-tree lst))
      ((eql type 'vector) (make-array (length lst) :initial-contents lst))
      ((eql type 'array) (make-array (list-dimensions lst) :initial-contents lst))
      ((subtypep type 'tensor) (copy! lst (zeros (list-dimensions lst) type)))
      (t (error "don't know how to copy a list to type ~a" type)))))

(defmethod copy! ((from t) (to cons))
  (labels ((mapcar! (f lst)
             (do ((lst* lst (cdr lst*)))
                 ((null lst*))
               (setf (car lst*) (funcall f (car lst*))))
             lst))
    (maptree-eki #'(lambda (x) (if (atom x) from (values x #'mapcar!))) to)))

(defmethod copy! ((x cons) (y tensor))
  (copy! (tensor-copy x 'array) y))

(defmethod copy! ((from array) (to array))
  (loop for idx being the idx from 0 below (array-dimensions to) with-iterator (:stride ((of-x (make-stride-rmj (coerce (array-dimensions to) '(simple-array index-type (*)))))))
        do (setf (row-major-aref to of-x) (row-major-aref from of-x)))
  to)

(defmethod copy! ((from t) (to array))
  (loop for idx being the idx from 0 below (array-dimensions to)
        with-iterator (:stride ((of-x (make-stride-rmj (coerce (array-dimensions to) 'index-store-vector)))))
        do (setf (row-major-aref to of-x) from))
  to)

(defmethod copy! ((arr array) (type symbol))
  (cond
    ((member type '(array nil)) (copy! arr (make-array (array-dimensions arr) :element-type (array-element-type arr))))
    ((member type '(list cons))
     (labels ((mtree (arr idx)
                (let ((n (length idx)))
                  (if (= n (array-rank arr)) (apply #'aref arr idx)
                      (loop :for i :from 0 :below (array-dimension arr n)
                            :collect (mtree arr (append idx (list i))))))))
       (mtree arr nil)))
    ((subtypep type 'tensor) (copy! arr (zeros (array-dimensions arr) type)))
    (t (error "don't know how to copy a list to type ~a" type))))

(define-tensor-generic tricopy! (a b uplo?)
  (:documentation "Copy upper order, lower order, or diagonal."))

(define-tensor-method tricopy! ((a dense-tensor :x) (b dense-tensor :x t) uplo?)
  `(ecase uplo?
     ,@(loop for op in '(:u :uo :l :lo)
             collect `(,op (dorefs (idx (dimensions b) :uplo? ,op)
                               ((refa a :type ,(cl :x))
                                (refb b :type ,(cl :x)))
                               (setf refb refa))))
     (:d
      (lety ((ss.a (vector-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides a)) :type index-type)
             (ss.b (vector-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides b)) :type index-type)
             (sto.a (store a) :type ,(store-type (cl :x)))
             (sto.b (store b) :type ,(store-type (cl :x))))
        (loop :repeat (the index-type (vector-min (dimensions b)))
              :for of.a :of-type index-type := (head a) :then (the index-type (+ of.a ss.a))
              :for of.b :of-type index-type := (head b) :then (the index-type (+ of.b ss.b))
              :do (setf (t.store-ref ,(cl :x) sto.b of.b) (t.store-ref ,(cl :x) sto.a of.a))))))
  'b)

(define-tensor-method tricopy! ((a t) (b dense-tensor :x) uplo?)
  `(let ((a (t.coerce ,(field-type (cl :x)) a)))
     (ecase uplo?
       ,@(loop for op in '(:u :uo :l :lo)
               collect `(,op (dorefs (idx (dimensions b) :uplo? ,op)
                                 ((refb b :type ,(cl :x)))
                                 (setf refb a))))
       (:d
        (lety ((ss.b (vector-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides b)) :type index-type)
               (sto.b (store b) :type ,(store-type (cl :x))))
          (loop :repeat (the index-type (vector-min (dimensions b)))
                :for of.b :of-type index-type := (head b) :then (the index-type (+ of.b ss.b))
                :do (setf (t.store-ref ,(cl :x) sto.b of.b) a)))))
     b))

(define-tensor-generic swap! (x y)
  (:documentation
   "(SWAP! x y)

  Given tensors X,Y, perform:

              X <-> Y

  and return Y.

  X, Y must have the same dimensions."))

(defmethod swap! :before ((x dense-tensor) (y dense-tensor))
  (assert (with-optimization (:speed 3 :safety 0) 
            (vector-eq (the index-store-vector (dimensions x)) 
                       (the index-store-vector (dimensions y)) 
                       #'=)) 
          nil
          'tensor-dimension-mismatch))

;;; BLAS
(deft/generic (t.blas-copy! #'subtypep) sym (x st-x y st-y))
(deft/method t.blas-copy! (sym blas-mixin) (x st-x y st-y)
  (let ((ncp? (null st-x)) (ftype (field-type sym)))
    (using-gensyms (decl (x y) (sto-x))
      `(let (,@decl)
         (declare (type ,sym ,@(unless ncp? `(,x)) ,y)
                  ,@(when ncp? `((type ,(field-type sym) ,x))))
         ,(recursive-append
           (when ncp? `(with-field-element ,sym (,sto-x ,x)))
           `(,(blas-func "copy" ftype)
             (the index-type (total-size ,y))
             ,(if ncp? sto-x `(t.store ,sym ,x))
             (the index-type ,(if ncp? 0 st-x))
             (t.store ,sym ,y)
             (the index-type ,st-y)))
         ,y))))

(deft/generic (t.copy! #'(lambda (a b) (strict-compare (list #'subtypep #'subtypep) a b))) (clx cly) (x y))
(deft/method t.copy! ((clx dense-tensor) (cly dense-tensor)) (x y)
  (using-gensyms (decl (x y) (ref-x ref-y idx))
    `(let* (,@decl)
       (declare (type ,clx ,x)
                (type ,cly ,y))
       (with-optimization (:speed 3 :safety 0)
        (dorefs (,idx (dimensions ,y))
            ((,ref-x ,x :type ,clx)
             (,ref-y ,y :type ,cly))
            (setf ,ref-y ,(if (and (subtypep (field-type clx) 'cl:real) (real-subtypep (field-type cly))) ;;Coercion messes up optimization in SBCL, so we specialize.
                              `(the ,(field-type cly) (complex (t.strict-coerce (,(field-type clx) ,(real-subtypep (field-type cly))) ,ref-x) (t.fid+ ,(real-subtypep (field-type cly)))))
                              (if (eql clx cly) ref-x `(t.strict-coerce (,(field-type clx) ,(field-type cly)) ,ref-x))))))
       ,y)))

(deft/method t.copy! ((clx t) (cly dense-tensor)) (x y)
  (using-gensyms (decl (x y) (ref-y idx cx))
    `(let* (,@decl
               (,cx (t.coerce ,(field-type cly) ,x)))
       (declare (type ,cly ,y)
                (type ,(field-type cly) ,cx))
       ;;This should be safe
       (with-optimization (:speed 3 :safety 0)
        (dorefs (,idx (dimensions ,y))
            ((,ref-y ,y :type ,cly))
            (setf ,ref-y ,cx)))
       ,y)))

;;
(deft/method (t.copy! #'(lambda (x) (hash-table-storep (first x)))) ((clx stride-accessor) (cly graph-accessor)) (x y)
  (using-gensyms (decl (x y) (rstd cstd rdat key value r c ii jj s? v vi vr vd i col-stop row))
    `(let (,@decl)
       (declare (type ,clx ,x) (type ,cly ,y))
       (let ((,cstd (strides ,x 1))
             (,rstd (strides ,x 0))
             (,rdat (make-array (dimensions ,x (if (slot-value ,y 'transposep) 0 1)) :initial-element nil)))
         (loop :for ,key :being :the :hash-keys :of (t.store ,clx ,x)
               :using (hash-value ,value)
               :do (letv* ((,c ,r (floor (the index-type ,key) ,cstd) :type index-type index-type)
                           (,r ,s? (floor (the index-type ,r) ,rstd) :type index-type index-type)
                           (,ii ,jj (if (slot-value ,y 'transposep) (values ,c ,r) (values ,r ,c)) :type index-type index-type))
                     (if (zerop ,s?)
                         (push (cons ,ii (t.strict-coerce (,(field-type clx) ,(field-type cly)) ,value)) (aref ,rdat ,jj))
                         (error "strides of the tensor are not canonical."))))
         (when (< (store-size ,y) (total-size ,x))
           (setf (slot-value ,y 'neighbours) (t.store-allocator index-store-vector (total-size ,x))
                 (slot-value ,y 'store) (t.store-allocator ,cly (total-size ,x))))
         (lety ((,vi (fence ,y) :type index-store-vector)
                     (,vr (δ-i ,y) :type index-store-vector)
                     (,vd (t.store ,cly ,y) :type ,(store-type cly)))
                    (setf (aref ,vi 0) 0)
                    (with-optimization (:speed 3 :safety 0)
                     (loop :for ,i :from 0 :below (length ,rdat)
                           :with ,col-stop := 0
                           :do (let ((,row (sort (aref ,rdat ,i) #'(lambda (x y) (< (the index-type x) (the index-type y))) :key #'car)))
                                 (loop :for (,r . ,v) :in ,row
                                       :do (locally
                                               (declare (type ,(field-type cly) ,v)
                                                        (type index-type ,r))
                                             (setf (aref ,vr ,col-stop) ,r)
                                             (t.store-set ,cly ,v ,vd ,col-stop)
                                             (incf ,col-stop)))
                                 (setf (aref ,vi (1+ ,i)) ,col-stop)))))
         ,y))))

(deft/method (t.copy! #'(lambda (x) (hash-table-storep (first x)))) ((clx stride-accessor) (cly dense-tensor)) (x y)
  (using-gensyms (decl (x y) (rstd cstd key value r c s?))
    `(let (,@decl)
       (declare (type ,clx ,x) (type ,cly ,y))
       (copy! (t.fid+ ,(field-type cly)) ,y)
       (let ((,cstd (strides ,x 1))
             (,rstd (strides ,x 0)))
         (loop :for ,key :being :the :hash-keys :of (t.store ,clx ,x)
               :using (hash-value ,value)
               :do (letv* ((,c ,r (floor (the index-type ,key) ,cstd) :type index-type index-type)
                           (,r ,s? (floor (the index-type ,r) ,rstd) :type index-type index-type))
                     (if (zerop ,s?)
                         (setf (ref ,y ,r ,c) (t.strict-coerce (,(field-type clx) ,(field-type cly)) ,value))
                         (error "strides of the tensor are not canonical."))))
         ,y))))

(deft/method (t.copy! #'(lambda (x) (hash-table-storep (second x)))) ((clx graph-accessor) (cly stride-accessor)) (x y)
  (using-gensyms (decl (x y) (key vi vr vd i j))
    `(let (,@decl)
       (declare (type ,clx ,x) (type ,cly ,y))
       (loop :for ,key :being :the :hash-keys :of (t.store ,cly ,y)
             :do (remhash ,key (t.store ,cly ,y)))
       (lety ((,vi (fence ,x) :type index-store-vector)
                   (,vr (δ-i ,x) :type index-store-vector)
                   (,vd (t.store ,clx ,x) :type ,(store-type clx)))
                  (if (slot-value ,x 'transposep)
                      (with-optimization (:speed 3 :safety 0)
                       (loop :for ,j :from 0 :below (1- (length ,vi))
                             :do (loop :for ,i :from (aref ,vi ,j) :below (aref ,vi (1+ ,j))
                                       :do (setf (ref ,y ,j (aref ,vr ,i)) (t.strict-coerce (,(field-type clx) ,(field-type cly)) (aref ,vd ,i))))))
                      (with-optimization (:speed 3 :safety 0)
                       (loop :for ,j :from 0 :below (1- (length ,vi))
                             :do (loop :for ,i :from (aref ,vi ,j) :below (aref ,vi (1+ ,j))
                                       :do (setf (ref ,y (aref ,vr ,i) ,j) (t.strict-coerce (,(field-type clx) ,(field-type cly)) (aref ,vd ,i))))))))
       ,y)))

(deft/method t.copy! ((clx graph-accessor) (cly dense-tensor)) (x y)
  (using-gensyms (decl (x y) (vi vr vd i j))
    `(let (,@decl)
       (declare (type ,clx ,x) (type ,cly ,y))
       (copy! (t.fid+ ,(field-type cly)) ,y)
       (lety ((,vi (fence ,x) :type index-store-vector)
                   (,vr (δ-i ,x) :type index-store-vector)
                   (,vd (t.store ,clx ,x) :type ,(store-type clx)))
                  (if (slot-value ,x 'transposep)
                      (with-optimization (:speed 3 :safety 0)
                       (loop :for ,j :from 0 :below (1- (length ,vi))
                             :do (loop :for ,i :from (aref ,vi ,j) :below (aref ,vi (1+ ,j))
                                       :do (setf (ref ,y ,j (aref ,vr ,i)) (t.strict-coerce (,(field-type clx) ,(field-type cly)) (t.store-ref ,clx ,vd ,i))))))
                      (with-optimization (:speed 3 :safety 0)
                       (loop :for ,j :from 0 :below (1- (length ,vi))
                             :do (loop :for ,i :from (aref ,vi ,j) :below (aref ,vi (1+ ,j))
                                       :do (setf (ref ,y (aref ,vr ,i) ,j) (t.strict-coerce (,(field-type clx) ,(field-type cly)) (t.store-ref ,clx ,vd ,i))))))))
       ,y)))

(deft/method t.copy! ((clx graph-tensor) (cly coordinate-tensor)) (x y)
  (using-gensyms (decl (x y) (idx i j m))
    (flet ((macro-expander (transpose-p)
             `((loop :for ,j :of-type index-type :from 0 :below (1- (length (memoizing (fence ,x))))
                     :do (loop :for ,m :of-type index-type :from (aref (memoizing (fence ,x)) ,j) :below (aref (memoizing (fence ,x)) (1+ ,j))
                               :do (lety ((,i (aref (memoizing (δ-i ,x) :type index-store-vector) ,m) :type index-type))
                                              (setf ;;set hash
                                               (aref ,idx 0) ,i (aref ,idx 1) ,j
                                               (aref (memoizing (slot-value ,y 'stride-hash) :type index-store-vector) ,m) (stride-hash ,idx (memoizing (strides ,y) :type index-store-vector))
                                               ;;set index
                                               (aref (memoizing (indices ,y) :type index-store-matrix) ,m 0) (aref ,idx ,(if transpose-p 1 0))
                                               (aref (memoizing (indices ,y) :type index-store-matrix) ,m 1) (aref ,idx ,(if transpose-p 0 1))
                                               ;;Set value
                                               (t.store-ref ,cly (memoizing (slot-value ,y 'store) :type ,(store-type cly)) ,m) (t.strict-coerce (,(field-type clx) ,(field-type cly)) (t.store-ref ,clx (memoizing (slot-value ,x 'store) :type ,(store-type clx)) ,i)))))))))
      `(let (,@decl)
         (declare (type ,clx ,x) (type ,cly ,y))
         (with-memoization ()
           (lety ((,idx (t.store-allocator index-store-vector 2)))
                      (if (slot-value ,x 'transposep) (with-optimization (:speed 3 :safety 0) ,@(macro-expander t)) (with-optimization (:speed 3 :safety 0) ,@(macro-expander nil)))
                      (setf (slot-value ,y 'tail) (total-size ,x))))
         ,y))))

(deft/method t.copy! ((clx hash-tensor) (cly coordinate-tensor)) (x y)
  (using-gensyms (decl (x y) (idx ii k v))
    `(let (,@decl)
       (declare (type ,clx ,x) (type ,cly ,y))
       (with-memoization ()
         (let ((,idx (t.store-allocator index-store-vector (memoizing (total-size ,x)))))
           (loop for ,k being the hash-keys of (store ,x)
                 for ,v being the hash-values of (store ,x)
                 do (setf (aref ,idx ,ii) ,k)
                 counting t into ,ii)
           (vector-copy (memoizing (total-size ,x)) (sort ,idx #'<) 0 (memoizing (slot-value ,y 'stride-hash) :type index-store-vector) 0))
         (setf (slot-value ,y 'tail) (memoizing (total-size ,x)))
         ;;
         (loop for ,k across (memoizing (slot-value ,y 'stride-hash)) 
               for ,ii from 0 below (memoizing (total-size ,x))
               do (vector-copy (memoizing (order ,x))
                               (the index-store-vector (invert-hash (- ,k (memoizing (head ,x))) (memoizing (slot-value ,x 'stride-pivot)) (memoizing (strides ,x)) (memoizing (dimensions ,x)))) 0
                               (memoizing (indices ,y) :type index-store-matrix) (* (memoizing (order ,x)) ,ii))
               do (setf (t.store-ref ,cly (memoizing (t.store ,cly ,y) :type ,(store-type cly)) ,ii)
                        (t.strict-coerce (,(field-type clx) ,(field-type cly)) (t.store-ref ,clx (memoizing (slot-value ,x 'store) :type ,(store-type clx)) ,k)))))
       ,y)))

(deft/method t.copy! ((clx coordinate-tensor) (cly dense-tensor)) (x y)
  (using-gensyms (decl (x y) (idx ii))
    `(let (,@decl)
       (declare (type ,clx ,x) (type ,cly ,y))
       (with-memoization ()
         (let ((,idx (t.store-allocator index-store-vector (order ,x))))
           (loop for ,ii from 0 below (slot-value ,x 'tail)
                 do (vector-copy (memoizing (order ,x)) (memoizing (indices ,x)) (* ,ii (memoizing (order ,x))) ,idx 0 :key #'row-major-aref :lock #'(setf aref))
                 do (setf (t.store-ref ,cly (memoizing (store ,y) :type ,(store-type cly)) (+ (memoizing (head ,y)) (stride-hash ,idx (memoizing (strides ,y)))))
                          (t.strict-coerce (,(field-type clx) ,(field-type cly)) (t.store-ref ,clx (memoizing (store ,x) :type ,(store-type clx)) ,ii))))))
       ,y)))

;; TODO 2025-12-08: 
(defmethod copy! :before ((x tensor) (y tensor))
  (assert (and (with-optimization (:speed 3 :safety 0) (vector-eq (dimensions x) (dimensions y) '=))) nil 'tensor-dimension-mismatch)
  (assert (<= (total-size x) (store-size y)) nil 'tensor-insufficient-store))

(define-tensor-method copy! ((x array) (y dense-tensor :y t))
  `(lety ((sto-y (store y) :type ,(store-type (cl :y))))
     (loop for idx being the index from 0 below (dimensions y) 
           with-iterator ((:stride ((of-y (strides y) (head y))
                                    (of-x (make-stride-rmj (coerce (array-dimensions x) 'index-store-vector))))))
           do (setf (t.store-ref ,(cl :y) sto-y of-y) (t.coerce ,(field-type (cl :y)) (row-major-aref x of-x))))
     y))

(define-tensor-method copy! ((x dense-tensor :x t) (y array))
  `(lety ((sto-x (store x) :type ,(store-type (cl :x))))
     (loop for idx being the index from 0 below (dimensions x) 
           with-iterator ((:stride ((of-x (strides x) (head x))
                                    (of-y (make-stride-rmj (coerce (array-dimensions y) 'index-store-vector))))))
           do (setf (row-major-aref y of-y) (t.store-ref ,(cl :x) sto-x of-x)))
     y))

(define-tensor-method copy! ((x tensor :x) (y tensor :y t))
  (recursive-append
   (when (and (eql (cl :x) (cl :y)) (subtypep (cl :y) 'blas-mixin))
     `(if-let ((strd (and (call-fortran? y (t.blas-lb ,(cl :y) 1)) (blas-copyablep x y))))
        (t.blas-copy! ,(cl :y) x (first strd) y (second strd))))
   `(t.copy! (,(cl :x) ,(cl :y)) x y))
  'y)

(define-tensor-method copy! ((x t) (y dense-tensor :y t))
  (recursive-append
   (when (subtypep (cl :y) 'blas-mixin)
     `(if-let ((strd (and (call-fortran? y (t.blas-lb ,(cl :y) 1)) (consecutive-storep y))))
        (t.blas-copy! ,(cl :y) (t.coerce ,(field-type (cl :y)) x) nil y strd)))
   `(t.copy! (t ,(cl :y)) x y)))

(define-tensor-method copy! ((x t) (y coordinate-tensor :y t))
  `(with-memoization ()
     (loop :for i :from 0 :below (slot-value y 'tail)
           :do (setf (t.store-ref ,(cl :y) (memoizing (store y) :type ,(store-type (cl :y))) i)
                     (memoizing (t.coerce ,(field-type (cl :y)) x) :type ,(field-type (cl :y)))))
     y))

(defmethod copy! ((tensor dense-tensor) (type symbol))
  (cond
    ((eql type 'array) (copy! tensor (make-array (vector-to-list (dimensions tensor)))))
    ((member type '(list cons))
     (labels ((mtree (arr idx)
                (let ((n (length idx)))
                  (if (= n (order arr)) (apply #'ref arr idx)
                      (loop :for i :from 0 :below (aref (dimensions arr) n)
                            :collect (mtree arr (append idx (list i))))))))
       (mtree tensor nil)))
    ((or (null type) (subtypep type 'dense-tensor))
     (copy! tensor (zeros (dimensions tensor) (or type (type-of tensor)))))
    (t (error "don't know how to copy ~a into ~a." (class-name (class-of tensor)) type))))

(defmethod copy! ((from foreign-dense-tensor) (to (eql nil)))
  (copy! from (tensor (field-type (class-of from)) 'simple-dense-tensor)))

(defmethod copy! ((tensor tensor) (type symbol))
  (cond
    ((or (null type) (subtypep type 'tensor))
     (let ((type (or type (type-of tensor))))
       (copy! tensor (zeros (dimensions tensor) type (if (subtypep type 'sparse-tensor) (total-size tensor))))))
    (t (error "don't know how to copy ~a into ~a." (class-name (class-of tensor)) type))))

#+nil
(defmethod copy-generic ((tensor sparse-tensor) type)
  (cond
    ((or (not type) (subtypep type 'sparse-tensor))
     (let ((ret (zeros (dimensions tensor) (or type (class-of tensor)) (store-size tensor))))
       (copy! tensor ret)))
    ((subtypep type 'standard-tensor)
     (let ((ret (zeros (dimensions tensor) type (store-size tensor))))
       (copy! tensor ret)))
    (t (error "don't know how to copy ~a into ~a." (class-name (class-of tensor)) type))))


;;
(define-tensor-generic tricopy! (a b uplo?)
  (:documentation "Copy upper order, lower order, or diagonal."))

(define-tensor-method tricopy! ((a dense-tensor :x) (b dense-tensor :x t) uplo?)
  `(ecase uplo?
     ,@(loop for op in '(:u :uo :l :lo)
             collect `(,op (dorefs (idx (dimensions b) :uplo? ,op)
                               ((refa a :type ,(cl :x))
                                (refb b :type ,(cl :x)))
                               (setf refb refa))))
     (:d
      (lety ((ss.a (vector-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides a)) :type index-type)
                  (ss.b (vector-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides b)) :type index-type)
                  (sto.a (store a) :type ,(store-type (cl :x)))
                  (sto.b (store b) :type ,(store-type (cl :x))))
                 (loop :repeat (the index-type (vector-min (dimensions b)))
                       :for of.a :of-type index-type := (head a) :then (the index-type (+ of.a ss.a))
                       :for of.b :of-type index-type := (head b) :then (the index-type (+ of.b ss.b))
                       :do (setf (t.store-ref ,(cl :x) sto.b of.b) (t.store-ref ,(cl :x) sto.a of.a))))))
  'b)

(define-tensor-method tricopy! ((a t) (b dense-tensor :x) uplo?)
  `(let ((a (t.coerce ,(field-type (cl :x)) a)))
     (ecase uplo?
       ,@(loop for op in '(:u :uo :l :lo)
               collect `(,op (dorefs (idx (dimensions b) :uplo? ,op)
                                 ((refb b :type ,(cl :x)))
                                 (setf refb a))))
       (:d
        (lety ((ss.b (vector-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides b)) :type index-type)
                    (sto.b (store b) :type ,(store-type (cl :x))))
                   (loop :repeat (the index-type (vector-min (dimensions b)))
                         :for of.b :of-type index-type := (head b) :then (the index-type (+ of.b ss.b))
                         :do (setf (t.store-ref ,(cl :x) sto.b of.b) a)))))
     b))
;;
(deft/generic (t.blas-swap! #'subtypep) sym (x st-x y st-y))
(deft/method t.blas-swap! (sym blas-mixin) (x st-x y st-y)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (x y))
      `(let (,@decl)
         (declare (type ,sym ,x ,y))
         (,(blas-func "swap" ftype)
          (total-size ,y)
          (the ,(store-type sym) (store ,x)) ,st-x
          (the ,(store-type sym) (store ,y)) ,st-y)
         ,y))))

(deft/generic (t.swap! #'subtypep) sym (x y))
(deft/method t.swap! (sym dense-tensor) (x y)
  (using-gensyms (decl (x y) (idx ref-x ref-y))
    `(let* (,@decl)
       (declare (type ,sym ,x ,y))
       (with-optimization (:speed 3 :safety 0)
        (dorefs (,idx (dimensions ,x))
            ((,ref-x ,x :type ,sym)
             (,ref-y ,y :type ,sym))
            (rotatef ,ref-x ,ref-y))
        ,y))))

(defmethod swap! :before ((x dense-tensor) (y dense-tensor))
  (assert (with-optimization (:speed 3 :safety 0) (vector-eq (the index-store-vector (dimensions x)) (the index-store-vector (dimensions y)) #'=)) nil
          'tensor-dimension-mismatch))

(define-tensor-method swap! ((x dense-tensor :x t) (y dense-tensor :x t))
  (recursive-append
   (when (subtypep (cl :x) 'blas-mixin)
     `(if-let ((strd (and (call-fortran? x (t.blas-lb ,(cl :x) 1)) (blas-copyablep x y))))
        (t.blas-swap! ,(cl :x) x (first strd) y (second strd)))))
  `(t.swap! ,(cl :x) x y))
