;;; var.lisp --- Math Variables

;; 

;;; Code:
(in-package :math/proto)

(defparameter *default-rcond* 1d-15
  "The default value of condition number to be used for determining the rank of a
matrix (used in gelsy).")

(defparameter *default-uplo* #\U
  "For routines which take symmetric (hermitian) matrices as arguments, this sets
the default argument for UPLO.")

