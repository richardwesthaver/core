;;; proto.lisp --- Tensor Protocols

;; Tensor Object API

;;; Commentary:

;; This file contains the 'high-level' tensor protocol. See meta.lisp for the
;; low-level bits.

;;; Code:
(in-package :obj/tensor)

;;; Vars
(defparameter *default-sparse-store-increment* 100
  "Determines the increment by which the store of a compressed sparse matrix is
increased, when it runs out of store.")

(defparameter *default-sparsity* 1/1000
  "Determines the default sparsity for a newly created sparse matrix, when the
number of non-zero is not specified.")

(defparameter *max-sparse-size* 10000
  "Upper bounds the store size for a newly created sparse matrix, when the number
of non-zero is not specified.")

;;Default ordering of strides
(eval-always
  (defparameter *default-stride-ordering* :col-major
    "Determines whether strides are row or column major by default.

(let ((*default-stride-ordering* :col-major))
   (make-real-tensor 10 10))
;; returns a 10x10 matrix in Column major order."))

(defparameter *default-tensor-type* 'real-tensor)

(defparameter *check-after-initializing-p* t
  "If non-nil, then check for invalid values in the field of the class in the
:after specialized method (if defined), else do nothing. One ought to be very
carful when doing, much of Matlisp's code is written on the assumption that
the fields of a tensor don't take invalid values; failing which case, may lead
to memory error. Use at your own risk.")

(defparameter *print-tensor-max-len* 10
  "Maximum number of elements in any particular argument to print.
Set this to T to print all the elements.")

(defparameter *print-tensor-max-args* 5
  "Maximum number of arguments of the tensor to print.
Set this to T to print all the arguments.")

(defparameter *print-tensor-indent* 0
  "Determines how many spaces will be printed before each row
of a matrix (default 0)")

;;; Conditions
(define-condition tensor-invalid-dimension-value (error)
  ((argument :initarg :argument)
   (dimension :initarg :dimension))
  (:report 
   (lambda (c s)
     (with-slots (argument dimension) c
       (format s "Invalid dimension arg: ~A~%dimension: ~A" argument dimension)))))

;;; Types
(deftype index-type () 'fixnum)

(deftype index-store-vector (&optional (size '*)) `(simple-array index-type (,size)))

;;; Generics
(defgeneric print-element (tensor
                           element stream)
  (:documentation "This generic function is specialized to TENSOR to print ELEMENT to STREAM.
Called by PRINT-TENSOR/MATRIX to format a tensor into the STREAM."))

(defgeneric size (obj)
  (:method ((obj sequence))
    (length obj))
  (:method ((arr array))
    (reduce #'* (array-dimensions arr))))

(defgeneric store-size (tensor)
  (:documentation "Returns the number of elements the store of the tensor can hold (which is not
necessarily equal to its vector length)."))

(defgeneric store-ref (tensor idx)
  (:documentation  "Generic serial read access to the store."))

(defgeneric (setf store-ref) (value tensor idx))

(defgeneric subtensor (tensor subscripts)
  (:documentation "Creates a new tensor data structure, sharing store with TENSOR but with
different strides and dimensions, as defined in the subscript-list SUBSCRIPTS.

Examples:
(defvar X (make-real-tensor 10 10 10))
;; X

;; Get (: 0 0)
(subtensor X '((nil nil . nil) (0 1 . nil) (0 1 . nil)))
;; Get (: 2:5 :)
(subtensor X '((nil nil . nil) (2 5 . nil)))
;; Get (: : 0:2:10) (0:10:2 = [i : 0 <= i < 10, i % 2 = 0])
(subtensor X '((nil nil . nil) (nil nil . nil) (0 10 . 2)))

Sadly in our parentheses filled world, this function has to be necessarily
verbose (unlike MATLAB, Python). However, this function has been designed with
the express purpose of using it with a Lisp reader macro. The slicing
semantics is essentially the same as MATLAB except for the zero-based
indexing."))

(defgeneric suptensor (tensor ord &optional start))

(defgeneric reshape (tensor dims)
  (:documentation "Reshape TENSOR to DIMS. This function expects all the strides to be of the
same sign when TENSOR is subtype of STANDARD-TENSOR."))

(defgeneric ref (tensor &rest subscripts)
  (:documentation "Return the element from TENSOR corresponding to SUBSCRIPTS"))

(defgeneric (setf ref) (value tensor &rest subscripts))

(defgeneric fc (x)
  (:method ((x complex))
    (conjugate x))
  (:method ((x real))
    x))
