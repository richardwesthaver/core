;;; proto.lisp --- Tensor Protocols

;; Tensor Object API

;;; Commentary:

;; This file contains the 'high-level' tensor protocol. See meta.lisp for the
;; low-level bits.

;;; Code:
(in-package :obj/tensor)

;;; Vars
(defparameter *sparse-tensor-realloc-on-setf* nil)
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

(defparameter *default-tensor-type* '(double-float))

(defparameter *tensor-safety-p* t
  "If non-nil, then check for invalid values in the field of the class in the
:after specialized method (if defined), else do nothing. One ought to be very
carful when doing, much of Matlisp's code is written on the assumption that
the fields of a tensor don't take invalid values; failing which case, may lead
to memory error. Use at your own risk.")

(defmacro without-tensor-safety (&rest body)
  `(let ((*tensor-safety-p* nil)) ,@body))

(defparameter *rcond-scale* 10
  "Factor by which the float-epsilon is to be scaled, so as to obtain a condition
number threshold, to be used for determining the rank of a matrix (used in gelsy).")

(defparameter *print-tensor-max-len* t
  "Maximum number of elements in any particular argument to print.
Set this to T to print all the elements.")

(defparameter *print-tensor-max-args* 5
  "Maximum number of arguments of the tensor to print.
Set this to T to print all the arguments.")

(defparameter *print-tensor-indent* 2
  "Determines how many spaces will be printed before each row
of a matrix (default 2)")

(defparameter *default-uplo* :l
  "For routines which take symmetric (hermitian) matrices as arguments, this sets
the default argument for UPLO.")

(defparameter *rcond-scale* 10
  "Factor by which the float-epsilon is to be scaled, so as to obtain a condition
number threshold, to be used for determining the rank of a matrix (used in
gelsy).")

(defparameter *default-uplo* :l
  "
For routines which take symmetric (hermitian) matrices as
arguments, this sets the default argument for UPLO.")

;; Level 1
(defparameter *real-l1-alien-threshold* 5000
  "If the size of the array is less than this parameter, the
lisp version of axpy is called in order to avoid FFI overheads.
The Fortran function is not called if the tensor does not have
a consecutive store.")

(defparameter *complex-l1-alien-threshold* 2500
  "If the size of the array is less than this parameter, the
lisp version of axpy is called in order to avoid FFI overheads.
The Fortran function is not called if the tensor does not have
a consecutive store.")

;; Level 2
(defparameter *real-l2-alien-threshold* 1000
  "If the maximum dimension in the MV is lower than this
parameter, then the lisp code is used by default, instead of
calling BLAS. Used to avoid the FFI overhead when calling
MM with small matrices. Note that if the dimensions do exceed
this lower  bound, then the Fortran function is called even if
the matrix has a BLAS incompatible stride (by doing a copy).

Default set with SBCL on x86-64 linux. A reasonable value
is something between 800 and 2000.")

(defparameter *complex-l2-alien-threshold* 600
  "If the maximum dimension in the MV is lower than this
parameter, then the lisp code is used by default, instead of
calling BLAS. Used to avoid the FFI overhead when calling
MM with small matrices. Note that if the dimensions do exceed
this lower bound, then the Fortran function is called even when
the matrices have a BLAS incompatible stride (by using a copy).

Default set with SBCL on x86-64 linux. A reasonable value
is something between 400 and 1000.")

;; Level 3
(defparameter *real-l3-alien-threshold* 50
  "If the maximum dimension in the MM is lower than this
parameter, then the lisp code is used by default, instead of
calling BLAS. Used to avoid the FFI overhead when calling
MM with small matrices.
Default set with SBCL on x86-64 linux. A reasonable value
is something between 20 and 200.")

(defparameter *complex-l3-alien-threshold* 30
  "If the maximum dimension in the MM is lower than this
parameter, then the lisp code is used by default, instead of
calling BLAS. Used to avoid the FFI overhead when calling
MM with small matrices.
Default set with SBCL on x86-64 linux. A reasonable value
is something between 20 and 200.")
