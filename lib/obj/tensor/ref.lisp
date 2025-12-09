;;; ref.lisp --- Tensor Ref

;; 

;;; Code:
(in-package :obj/tensor)

(labels ((array-subs (obj subscripts)
           (let ((subs (etypecase (car subscripts)
                         (number subscripts)
                         (cons (car subscripts))
                         (vector (vector-to-list (car subscripts))))))
             ;; TODO verify iter replacement works here
             (loop for s on subs
                   for i from 0
                   when (< (car s) 0)
                   do (rplaca s (modproj (car s) (array-dimension obj i) nil)))
             subs)))
  (defmethod ref ((obj array) &rest subscripts)
    (apply #'aref obj (array-subs obj subscripts)))
  (defmethod (setf ref) (value (obj array) &rest subscripts)
    (apply #'(setf aref) value obj (array-subs obj subscripts))))

(labels ((list-subs (obj subscripts)
           (let ((subs (etypecase (car subscripts)
                         (number subscripts)
                         (cons (car subscripts))
                         (vector (vector-to-list (car subscripts))))))
             (assert (= (length subs) 1) nil 'invalid-arguments) (setf subs (car subs))
             (when (< subs 0) (setf subs (modproj subs (length obj))))
             subs)))
  (defmethod ref ((obj cons) &rest subscripts)
    (cond
      ((and (not (cdr subscripts)) (symbolp (first subscripts))) (getf obj (first subscripts)))
      (t (elt obj (list-subs obj subscripts)))))
  (defmethod (setf ref) (value (obj cons) &rest subscripts)
    (cond
      ((and (not (cdr subscripts)) (symbolp (first subscripts))) (setf (getf obj (first subscripts)) value))
      (t (setf (elt obj (list-subs obj subscripts)) value)))))

;; TODO
;; (defmethod ref :before ((obj hash-table) &rest subscripts)
;;   (assert (and (first subscripts) (not (cdr subscripts))) nil 'invalid-arguments))

(defmethod ref ((obj hash-table) &rest subscripts)
  (gethash (car subscripts) obj))

(defmethod (setf ref) (value (obj hash-table) &rest subscripts)
  (setf (gethash (car subscripts) obj) value))
