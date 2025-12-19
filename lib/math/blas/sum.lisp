;;; sum.lisp --- Tensor Summation

;; 

;;; Code:
(in-package :math/blas)

(deft/generic (t.sum #'subtypep) sym (x ret &optional axis))
(deft/method t.sum (sym dense-tensor) (x ret &optional (axis 0))
  (if (null ret)
      (using-gensyms (decl (x) (ret idx ref))
        `(let (,@decl
               (,ret (t.fid+ ,(field-type sym))))
           (dorefs (,idx (dimensions ,x))
                   ((,ref ,x :type ,sym))
                   (setf ,ret (t.f+ ,(field-type sym) ,ret ,ref)))
           ,ret))
      (using-gensyms (decl (x axis ret))
        (with-gensyms (view argstd)
          `(let* (,@decl)
             (declare (type ,sym ,x ,ret)
                      (type index-type ,axis))
             (let ((,view (slice~ ,x ,axis))
                   (,ret (if (= (order ,ret) (order ,x)) (slice~ ,ret ,axis) ,ret))
                   (,argstd (strides ,x ,axis)))
               (declare (type ,sym ,view)
                        (type index-type ,argstd))
               (loop :for i :from 0 :below (dimensions ,x ,axis)
                  :do (progn
                        (axpy! (t.fid* ,(field-type sym)) ,view ,ret)
                        (incf (slot-value ,view 'head) ,argstd))))
             ,ret)))))

(defun reduce-check (x y axis)
  (declare (base-tensor x y))
  (let ((axis (modproj (or axis 0) (order x) nil 0)))
    (cond
      ((= (1- (order x)) (order y))
       (loop :for i :of-type index-type :from 0 :below (order x)
          :and j :of-type index-type := 0 :then (if (= i axis) j (1+ j))
          :always (or (= i axis) (= (dimensions x i) (dimensions y j)))))
      ((= (order x) (order y))
       (loop :for i :from 0 :below (order x)
          :always (if (= i axis) (= (dimensions y i) 1) (= (dimensions x i) (dimensions y i))))))))

(eval-always
  (defgeneric tensor-sum! (x y &optional axis)
    (:documentation "
  (TENSOR-SUM! x y [axis 0] [beta 0])

       --
  y <- \  x(:, : ..., i, :, :..)
       /_
        i
  where the index to be summed over is chosen using AXIS.
")
    (:generic-function-class tensor-method-generator)))

(defmethod tensor-sum! :before ((x dense-tensor) (y dense-tensor) &optional (axis 0))
  (assert (reduce-check x y axis) nil 'tensor-dimension-mismatch))

(define-tensor-method tensor-sum! ((x dense-tensor :x) (y dense-tensor :y) &optional (axis 0))
  `(t.sum ,(cl :y) x (copy! (t.fid+ ,(field-type (cl :y))) y) axis))

(define-tensor-method tensor-sum! ((x dense-tensor :x) (y (eql nil)) &optional (axis 0))
  `(declare (ignore axis))
  `(t.sum ,(cl :x) x nil))

(eval-always
  (defgeneric prod! (x y &optional axis)
    (:documentation "
  (PROD! x y [axis 0])
       __
  y <- || x(:, : ..., i, :, :..)
        i
  where the index to be summed over is chosen using @arg{axis}.
")
    (:generic-function-class tensor-method-generator)))

(defmethod prod! :before ((x dense-tensor) (y dense-tensor) &optional (axis 0))
  (assert (reduce-check x y axis) nil 'tensor-dimension-mismatch))


(labels ((*-ify (code)
           (maptree `(t.fid+ t.f+ axpy!)
                    #'(lambda (x) (values (ecase (car x)
                                            (t.fid+ `(t.fid* ,@(cdr x)))
                                            (axpy! `(scal! ,@(cddr x)))
                                            (t.f+ `(t.f* ,@(cdr x))))
                                          #'mapcar))
             (macroexpand-1 code))))
  ;;Don't you just love lisp :)
  (define-tensor-method prod! ((x dense-tensor :x) (y dense-tensor :y) &optional (axis 0))
    (*-ify `(t.sum ,(cl :y) x (copy! (t.fid+ ,(field-type (cl :y))) y) axis)))
  (define-tensor-method prod! ((x dense-tensor :x) (y (eql nil)) &optional axis)
    `(declare (ignore axis))
    (*-ify `(t.sum ,(cl :x) x nil))))

(defgeneric tensor-sum (x &optional axis preserve-rank?)
  (:method ((x dense-tensor) &optional (axis 0) (preserve-rank? nil))
    (if axis
        (let ((axis (modproj axis (order x))))
          (tensor-sum! x (let ((d (loop for ele across (dimensions x)
                                        for i from 0
                                        when (if preserve-rank? t (/= i axis))
                                        collect (if (= i axis) 1 ele))))
                           (and d (zeros d (class-of x))))
                axis))
        (tensor-sum! x nil)))
  (:method ((x number) &optional axis preserve-rank?)
    (declare (ignore axis preserve-rank?))
    x)
  (:method ((x sequence) &optional axis preserve-rank?)
    (declare (ignore axis preserve-rank?))
    (reduce #'+ x)))

(defgeneric mean (x &optional axis preserve-rank?)
  (:method ((x dense-tensor) &optional (axis 0) (preserve-rank? nil))
    (let ((s (tensor-sum x axis preserve-rank?))
          (n.d (if axis (/ (dimensions x axis)) (total-size x))))
      (if (numberp s) (* s n.d)
          (scal! n.d s))))
  (:method ((x number) &optional axis preserve-rank?)
    (declare (ignore axis preserve-rank?))
    x)
  (:method ((x sequence) &optional axis preserve-rank?)
    (declare (ignore axis preserve-rank?))
    (/ (tensor-sum x) (length x))))

#+nil
(defun cov (x &optional (axis -1) bias)
  (declare (type tensor-matrix x)
           (type index-type axis))
  (let* ((d (dimensions x (ecase (modproj axis 2 nil) (0 1) (1 0))))
         (μ (mean x axis)) (δ (zeros d (type-of x))))
    (loop for xi slicing x along axis 
          with ret = (zeros (list d d) (type-of x))
          do (ger! 1 (axpy! -1 μ (copy! xi δ)) δ ret)
          finally (return (values (scal! (/ (- (dimensions x axis) (if bias 0 1))) ret) μ)))))

(defgeneric prod (x &optional axis preserve-rank?)
  (:method ((x dense-tensor) &optional (axis 0) (preserve-rank? nil))
    (if axis
        (let ((axis (modproj axis (order x))))
          (prod! x (let ((dims (loop :for ele :across (dimensions x)
                                 :for i := 0 :then (1+ i)
                                 :when (if preserve-rank? t (/= i axis)) :collect (if (= i axis) 1 ele))))
                    (and dims (zeros dims (class-of x))))
                axis))
        (prod! x nil)))
  (:method ((x number) &optional axis preserve-rank?)
    (declare (ignore axis preserve-rank?))
    x)
  (:method ((x sequence) &optional axis preserve-rank?)
    (declare (ignore axis preserve-rank?))
    (reduce #'cl:* x)))
