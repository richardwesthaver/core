;;; copy.lisp --- Tensor COPY

;; 

;;; Code:
(in-package :obj/tensor)

(defgeneric copy! (from to)
  (:documentation
   "
  (COPY! x y)

  Copy the contents of X into Y. Return Y.
")
  (:method :before ((x array) (y array))
     (assert (equal (array-dimensions x) (array-dimensions y)) nil 'dimension-mismatch))
  (:method  :before ((x array) (y tensor))
     (assert (equal (array-dimensions x) (dimensions y t)) nil 'dimension-mismatch))
  (:method :before ((x tensor) (y array))
     (assert (equal (array-dimensions y) (dimensions x t)) nil 'dimension-mismatch))  
  (:method :before ((x cons) (y cons))
     (assert (= (length x) (length y)) nil 'dimension-mismatch))
  (:generic-function-class tensor-method-generator))

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

(defgeneric tricopy! (a b uplo?)
  (:documentation "Copy upper order, lower order, or diagonal.")
  (:generic-function-class tensor-method-generator))

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
  `(let ((a (t/coerce ,(field-type (cl :x)) a)))
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

(deft/generic (t.swap! #'subtypep) sym (x y))
(deft/method t.swap! (sym dense-tensor) (x y)
  (using-gensyms (decl (x y) (idx ref-x ref-y))
    `(let* (,@decl)
       (declare (type ,sym ,x ,y))
       (very-quickly
         (dorefs (,idx (dimensions ,x))
                 ((,ref-x ,x :type ,sym)
                  (,ref-y ,y :type ,sym))
           (rotatef ,ref-x ,ref-y))
         ,y))))

(defgeneric swap! (x y)
  (:documentation
"(SWAP! x y)

  Given tensors X,Y, perform:

              X <-> Y

  and return Y.

  X, Y must have the same dimensions.")
  (:generic-function-class tensor-method-generator))

(defmethod swap! :before ((x dense-tensor) (y dense-tensor))
  (assert (with-optimization (:speed 3 :safety 0) 
            (vector-eq (the index-store-vector (dimensions x)) 
                       (the index-store-vector (dimensions y)) 
                       #'=)) 
          nil
          'tensor-dimension-mismatch))
