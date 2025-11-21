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

(definline .copy (obj &optional type)
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
  (copy! (.copy x 'array) y))

(defmethod copy! ((from array) (to array))
  ;; TODO
  ;; (iter (for-mod idx from 0 below (array-dimensions to) with-iterator ((:stride ((of-x (make-stride-rmj (coerce (array-dimensions to) '(simple-array index-type (*)))))))))
  ;; (setf (row-major-aref to of-x) (row-major-aref from of-x)))
  to)

(defmethod copy! ((from t) (to array))
  ;; TODO
  ;; (iter (for-mod idx from 0 below (array-dimensions to) with-iterator ((:stride ((of-x (make-stride-rmj (coerce (array-dimensions to) 'index-store-vector)))))))
  ;; (setf (row-major-aref to of-x) from))
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

(defgeneric swap! (x y)
  (:documentation
"(SWAP! x y)

  Given tensors X,Y, perform:

              X <-> Y

  and return Y.

  X, Y must have the same dimensions.")
  (:generic-function-class tensor-method-generator))
