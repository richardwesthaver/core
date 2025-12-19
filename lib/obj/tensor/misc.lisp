;;; misc.lisp --- Miscellaneous Tensor Utils

;; 

;;; Code:
(in-package :obj/tensor)

(definline quaternion-vectorp (x)
  (declare (type dense-tensor x))
  (and (= (order x) 1) (= (dimensions x 0) 4) (= (strides x 0) 1)))
(deftype quaternion-vector (&optional (type '* type-p))
  (if type-p
      `(and ,(tensor type) (satisfies quaternion-vectorp))
      `(satisfies quaternion-vectorp)))

(definline r3-vectorp (x)
  (declare (type dense-tensor x))
  (and (= (order x) 1) (= (dimensions x 0) 3) (= (strides x 0) 1)))
(deftype r3-vector (&optional (type '* type-p))
  (if type-p
      `(and ,(tensor type) (satisfies r3-vectorp))
      `(satisfies r3-vectorp)))

(defun consecutive-storep (tensor)
  (declare (type stride-accessor tensor))
  (with-memoization ((memos tensor))
    (memoizing
     (let ((tensor tensor))
       (letv* ((sort-std std-perm (with-optimization (:speed 3 :safety 0) (sort-index (the index-store-vector (copy-seq (strides tensor))) #'<)) :type index-store-vector index-store-vector)
               (perm-dims (with-optimization (:speed 3 :safety 0) (apply-action! (copy-seq (the index-store-vector (dimensions tensor))) std-perm)) :type index-store-vector))
         (with-optimization (:speed 3 :safety 0)
           (loop
             :for so-st :across sort-std
             :for so-di :across perm-dims
             :and accumulated-off := (aref sort-std 0) :then (the index-type (* accumulated-off so-di))
             :unless (= so-st accumulated-off) :do (return (values nil perm-dims sort-std std-perm))
             :finally (return (values (aref sort-std 0) perm-dims sort-std std-perm)))))))))

(definline blas-func (name type)
  "Return the name of a given BLAS/LAPACK function whose base name is NAME
operating on the type TYPE."
  (let ((prefix (cond
                  ((eq type 'single-float) "s")
                  ((eq type 'double-float) "d")
                  ((equal type '(complex single-float)) "c")
                  ((equal type '(complex double-float)) "z")
                  (t (error "Unknown BLAS type: ~S" type)))))
    (concatenate 'string prefix name)))

(definline blas-copyablep (ten-a ten-b)
  (declare (type stride-accessor ten-a ten-b))
  (when (= (order ten-a) (order ten-b))
    (letv* ((csto-a? pdims-a tmp perm-a (consecutive-storep ten-a) :type t index-store-vector nil index-store-vector)
            (csto-b? pdims-b tmp perm-b (consecutive-storep ten-b) :type t index-store-vector nil index-store-vector))
      (when (and csto-a? csto-b? (with-optimization (:speed 3 :safety 0) (vector-eq perm-a perm-b)) (with-optimization (:speed 3 :safety 0) (vector-eq pdims-a pdims-b)))
        (list csto-a? csto-b?)))))

(definline call-fortran? (x lb)
  (declare (type stride-accessor x))
  (> (total-size x) lb))

(definline fortran-nop (op)
  (ecase op (#\T #\N) (#\N #\T)))

(definline change-jobchar (op job)
  (cart-case (op job)
    ;;((null (#\N #\T #\C)) job)
    (((transpose transpose~) (#\N #\T)) (fortran-nop job))
    (((ctranspose ctranspose~) #\N) #\C)
    (((ctranspose ctranspose~) #\C) #\N)))

(definline fortran-nuplo (op)
  (ecase op (#\U #\L) (#\L #\U)))

(definline split-job (job)
  (declare (type symbol job))
  (lety ((name (symbol-name job) :type string))
    (loop :for x :across name :collect (char-upcase x))))

(definline flip-major (job)
  (declare (type symbol job))
  (case job
    (:row-major :col-major)
    (:col-major :row-major)))

(definline blas-matrix-compatiblep (matrix &optional (op #\N))
  (declare (type stride-accessor matrix)
           (type character op))
  (assert (tensor-matrixp matrix) nil 'tensor-not-matrix)
  (lety* ((stds (strides matrix) :type index-store-vector)
          (rs (aref stds 0) :type index-type)
          (cs (aref stds 1) :type index-type))
    ;;Note that it is not required that (rs = nc * cs) or (cs = nr * rs)
    (cond
      ;;The ordering of these conditions is important to meet certain assumed conditions
      ;;in GEMM, when MATRIX has strides of the form #(1 1).
      ((and (= rs 1) (> cs 0)) (values cs op :col-major))
      ((and (char/= op #\C) (= cs 1) (> rs 0)) (values rs (fortran-nop op) :row-major)))))

(defmacro with-rowm (&rest body)
  `(let ((*default-stride-ordering* :row-major))
     ,@body))

(defmacro with-colm (&rest body)
  `(let ((*default-stride-ordering* :col-major))
     ,@body))

(defmacro with-columnification (((&rest input) (&rest output)) &rest body)
  (let ((input-syms (mapcar #'(lambda (x) (gensym (symbol-name (car x)))) input))
        (output-syms (mapcar #'(lambda (mat) (gensym (symbol-name mat))) output)))
    (with-gensyms (stack)
      `(let ((,stack nil))
         (declare (ignorable ,stack))
         (let (,@(mapcar #'(lambda (x sym) (destructuring-bind (mat job) x
                                             `(,sym (if (blas-matrix-compatiblep ,mat ,job) ,mat (with-colm (tensor-copy ,mat))))))
                         input input-syms)
               ,@(mapcar #'(lambda (mat sym) `(,sym (if (eql (nth-value 2 (blas-matrix-compatiblep ,mat #\N)) :col-major) (progn (push nil ,stack) ,mat)
                                                        (with-colm (push t ,stack) (tensor-copy ,mat))))) output output-syms))
           (symbol-macrolet (,@(mapcar #'(lambda (mat sym) `(,mat ,sym)) (append (mapcar #'car input) output) (append input-syms output-syms)))
             ,@body)
           ,@(mapcar #'(lambda (mat sym) `(when (pop ,stack) (copy! ,sym ,mat))) (reverse output) (reverse output-syms))
           nil)))))

(definline pflip.f->l (uidiv &optional uplo)
  (declare (type (simple-array (signed-byte 32) (*)) uidiv))
  (let ((ret (make-array (length uidiv) :element-type 'index-type)))
    (declare (type index-store-vector ret))
    (case uplo
      (:u (with-optimization (:speed 3 :safety 0)
            (loop :with i :of-type index-type := 0
                  :do (if (> (aref uidiv i) 0)
                          (setf (aref ret i) (1- (aref uidiv i)))
                          (setf (aref ret i) (1- (- (aref uidiv i)))
                                (aref ret (incf i)) i))
                  :do (incf i) :when (>= i (length uidiv)) :do (return))))
      (:l (with-optimization (:speed 3 :safety 0)
            (loop :with i :of-type index-type := 0
                  :do (if (> (aref uidiv i) 0)
                          (setf (aref ret i) (1- (aref uidiv i)))
                          (setf (aref ret i) i
                                (aref ret (incf i)) (1- (- (aref uidiv i)))))
                  :do (incf i) :when (>= i (length uidiv)) :do (return))))
      (t (with-optimization (:speed 3 :safety 0)
           (loop :for i :from 0 :below (length uidiv)
                 :do (setf (aref ret i) (1- (aref uidiv i)))))))
    ret))

(definline pflip.l->f (idiv)
  (declare (type index-store-vector idiv))
  (let ((ret (make-array (length idiv) :element-type '(signed-byte 32))))
    (declare (type (simple-array (signed-byte 32) (*)) ret))
    (with-optimization (:speed 3 :safety 0)
      (loop :for i :from 0 :below (length idiv)
            :do (setf (aref ret i) (1+ (aref idiv i)))))
    ret))

(defun realtype-max (lst)
  (let ((x (first (sort lst
                        #'(lambda (x y)
                            (cart-typecase (x y)
                              ((integer integer) (> (integer-length x) (integer-length y)))
                              ((ratio integer) t)
                              ((float float) (> (float-digits x) (float-digits y)))
                              ((float t) t)))))))
    (etypecase x
      ((or integer float) (type-of x))
      (ratio 'rational))))

(defun real-type-max (x y)
  (let ((ret (cart-etypecase (x y)
               ((integer integer) (type-of (if (> (integer-length x) (integer-length y)) x y)))
               ((ratio integer) 'ratio)
               ((float float) (type-of (if (> (float-digits x) (float-digits y)) x y)))
               ((float t) (type-of x)))))
    (if (eq ret 'ratio) 'rational ret)))

(defun range (start end &optional h_ list-outputp &aux (h (or h_ 1)))
  (declare (type real start end h))
  (let ((quo (ceiling (if (> start end) (- start end) (- end start)) h))
        (h (if (> start end) (- h) h)))
    (if (= quo 0) nil
        (if (not list-outputp)
            (let* ((type (realtype-max (list h start end (+ h start) (- end h)))))
              (mapsor! (let ((ori (coerce start type)) (h (coerce h type)))
                         (lambda (idx y) (declare (ignore idx y)) (prog1 ori (incf ori h))))
                       nil (zeros quo (tensor type))))
            (loop :for i :from 0 :below quo
                  :for ori := start :then (+ ori h)
                  :collect ori)))))

(defun linspace (start end &optional num-points list-outputp)
  (let* ((num-points (floor (or num-points (1+ (abs (- start end))))))
         (h (/ (- end start) (1- num-points))))
    (range start (+ h end) (abs h) list-outputp)))

;;This will only work if type is a dense-tensor
(defun ones (dims &optional (type *default-tensor-type*))
  (the dense-tensor (zeros dims type 1)))

(defun eye! (tensor)
  (tricopy! 1 (copy! 0 tensor) :d))

(defun eye (dims &optional (type *default-tensor-type*))
  (tricopy! 1 (zeros dims type) :d))

(defun diag (tensor &optional (order 2))
  (declare (type (and tensor-vector dense-tensor) tensor))
  (tricopy! tensor (zeros (make-list order :initial-element (dimensions tensor 0)) (type-of tensor)) :d))

(defun diag~ (a &optional bias)
  (declare (type dense-tensor a))
  (letv* ((off dim
               (if bias
                   (let ((bias (etypecase bias
                                 (index-type (idxv 0 bias))
                                 (list (coerce bias 'index-store-vector))
                                 (vector (make-array (length bias) :element-type 'index-type :initial-contents bias)))))
                     (assert (= (length bias) (order a)) nil 'tensor-index-rank-mismatch)
                     (letv* ((min (vector-min bias)))
                       (loop for di across (dimensions a)
                             for i = 0 then (incf i)
                             do (decf (aref bias i) min)
                             minimizing (- di (aref bias i)) into dim
                             summing (* (strides a i) (aref bias i)) into off
                             finally
                                (progn
                                  (assert (< 0 dim) nil 'tensor-dimension-mismatch)
                                  (return (values (+ (head a) off) dim))))))
                   (values (head a) (vector-min (dimensions a))))))
    (without-tensor-safety
        (make-instance (class-of a)
          :dimensions (coerce (list dim) 'index-store-vector)
          :strides (coerce (list (vector-foldr #'+ (strides a))) 'index-store-vector)
          :head off :store (store a) :parent a))))

(defun (setf diag~) (value tensor &optional bias) (copy! value (diag~ tensor bias)))

