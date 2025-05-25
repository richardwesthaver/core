;;; util.lisp --- Tensor Utils

;; 

;;; Code:
(in-package :obj/tensor)
;;; Utils
(defmacro with-no-init-checks (&body body)
  `(let ((*check-after-initializing-p* nil))
     ,@body))

(defun subfieldp (a b)
  (subtypep (field-type a) (field-type b)))

(defun t.zeros (ty dims &optional initial-element)
  (let* ((adims (make-index-store dims)))
    (declare (type index-store-vector adims))
    (multiple-value-bind (astrs sizs) (make-stride adims)
      (declare (type index-store-vector astrs))
      (make-instance ty
        :dimensions adims
        :head 0
        :strides astrs
        :store (t.store-allocator ty sizs initial-element)))))

;; (deft t.zeros (class coordinate-sparse-tensor) (dims &optional nz)
;;   (with-gensyms (astrs adims sizs)
;;     `(let* ((,adims (make-index-store ,dims)))
;;        (declare (type index-store-vector ,adims))
;;        (multiple-value-bind (,astrs ,sizs) (make-stride-cmj ,adims)
;;          (declare (type index-store-vector ,astrs))
;;          (make-instance ',class
;;            :dimensions ,adims
;;            :strides ,astrs
;;            :store (t.store-allocator ,class ,sizs ,nz))))))

;; (deft t.zeros (class compressed-sparse-matrix) (dims &optional nz)
;;   (with-gensyms (dsym)
;;     `(let ((,dsym ,dims))
;;        (destructuring-bind (vr vd) (t.store-allocator ,class ,dsym ,nz)
;;          (make-instance ',class
;;            :dimensions (make-index-store ,dims)
;;            :neighbour-start (allocate-index-store (1+ (second ,dsym)))
;;            :neighbour-id vr
;;            :store vd)))))

(defgeneric %zeros (dims dtype &optional initial-element)
  (:documentation "internal dispatch for ZEROS.")
  (:method ((dims cons) (dtype t) &optional initial-element)
    ;; (assert (member dtype *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class dtype)
        (if initial-element
            (t.zeros dtype dims initial-element)
            (t.zeros dtype dims))))

(definline zeros (dims &key (type *default-tensor-type*) (initial-element 0))
"Create a tensor with dimensions @arg{dims} of class @arg{dtype}.
The optional argument @arg{initial-element} is used in two completely
incompatible ways.

If @arg{dtype} is a dense tensor, then @arg{initial-element}, is used to
initialize all the elements. If @arg{dtype} is however, a sparse tensor,
it is used for computing the number of nonzeros slots in the store.

Example:
(zeros 3)
#<REAL-TENSOR #(3)
  0.0000      0.0000      0.0000     
>

(zeros 3 'complex-tensor 2)
#<COMPLEX-TENSOR #(3)
  2.0000      2.0000      2.0000     
>

(zeros '(10000 10000) 'real-compressed-sparse-matrix 10000)
#<REAL-COMPRESSED-SPARSE-MATRIX #(10000 10000), store-size: 10000>"
  (with-no-init-checks
    (etypecase dims
      (cons
       (zeros-generic dims type initial-element))
      (vector
       (zeros-generic (vector-to-list dims) type initial-element))
      (fixnum
       (zeros-generic (list dims) type initial-element)))))

(declaim (ftype (function ((or cons vector fixnum) &key (type t) (initial-element t)) base-tensor) zeros))

(defmacro with-rowm (&rest body)
  `(let ((*default-stride-ordering* :row-major))
     ,@body))

(defmacro with-colm (&rest body)
  `(let ((*default-stride-ordering* :col-major))
     ,@body))


(definline nrows (matrix)
  (aref (the index-store-vector (dimensions matrix)) 0))

(definline ncols (matrix)
  (aref (the index-store-vector (dimensions matrix)) 1))

(definline row-stride (matrix)
  (aref (the index-store-vector (strides matrix)) 0))

(definline col-stride (matrix)
  (aref (the index-store-vector (strides matrix)) 1))

(definline tensor-square-matrixp (matrix)
  (and (tensor-matrixp matrix) (tensor-squarep matrix)))
