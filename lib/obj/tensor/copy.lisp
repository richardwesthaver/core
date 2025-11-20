;;; copy.lisp --- Tensor COPY

;; 

;;; Code:
(in-package :obj/tensor)

;;; COPY
(defmethod copy ((from array) (to array))
  (let ((lst (make-list (array-rank to))))
    (mod-dotimes (idx (make-index-store (array-dimensions to)))
      do (progn
           (copy-vector-to-list idx lst)
           (setf (apply #'aref to lst) (apply #'aref from lst)))))
  to)

(defmethod copy ((from t) (to array))
  (let ((lst (make-list (array-rank to))))
    (mod-dotimes (idx (make-index-store (array-dimensions to)))
      do (progn
           (copy-vector-to-list idx lst)
           (setf (apply #'aref to lst) from)))
    to))

;;
(defmethod copy :before ((x array) (y tensor))
  (assert (tree-equal (array-dimensions x) (vector-to-list (dimensions y)))
          nil 'dimension-mismatch))
(defmethod copy :before ((x tensor) (y array))
  (assert (tree-equal (array-dimensions y) (vector-to-list (dimensions x)))
          nil 'dimension-mismatch))

(defmethod copy ((x array) (y tensor))
  (let ((clname (class-name (class-of y))))
    ;; (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod copy ((x array) (y ,clname))
        (lety ((sto-y (store y) :type (simple-array ,(store-element-type clname)))
               (lst (make-list (array-rank x)) :type cons))
              (mod-dotimes (idx (dimensions y))
                with (linear-sums
                      (of-y (strides y) (head y)))
                do (t.store-set ,clname (t.coerce ,(field-type clname) (apply #'aref x (copy-vector-to-list idx lst))) sto-y of-y)))
        y))
    (copy x y)))

(defmethod copy ((x tensor) (y array))
  (let ((clname (class-name (class-of x))))
    (compile-and-eval
     `(defmethod copy ((x ,clname) (y array))
        (let-typed ((sto-x (store x) :type (simple-array ,(store-element-type clname)))
                    (lst (make-list (array-rank y)) :type cons))
                   (mod-dotimes (idx (dimensions x))
                     with (linear-sums
                           (of-x (strides x) (head x)))
                     do (setf (apply #'aref y (copy-vector-to-list idx lst)) (t.store-ref ,clname sto-x of-x))))
        y))
    (copy x y)))

(defmethod copy ((x cons) (y tensor))
  ;;You seriously weren't expecting efficiency were you :) ?
  (let ((arr (make-array (list-dimensions x) :initial-contents x)))
    (copy arr y)))
