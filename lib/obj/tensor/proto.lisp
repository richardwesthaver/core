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

(defparameter *tensor-safety-p* t
  "If non-nil, then check for invalid values in the field of the class in the
:after specialized method (if defined), else do nothing. One ought to be very
carful when doing, much of Matlisp's code is written on the assumption that
the fields of a tensor don't take invalid values; failing which case, may lead
to memory error. Use at your own risk.")

(defparameter *print-tensor-max-len* t
  "Maximum number of elements in any particular argument to print.
Set this to T to print all the elements.")

(defparameter *print-tensor-max-args* 5
  "Maximum number of arguments of the tensor to print.
Set this to T to print all the arguments.")

(defparameter *print-tensor-indent* 2
  "Determines how many spaces will be printed before each row
of a matrix (default 2)")

;;; Conditions
(define-condition tensor-invalid-dimension-value (error)
  ((argument :initarg :argument)
   (dimension :initarg :dimension))
  (:report 
   (lambda (c s)
     (with-slots (argument dimension) c
       (format s "Invalid dimension arg: ~A~%dimension: ~A" argument dimension)))))
