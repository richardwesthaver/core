;;; math/tests.lisp --- MATH tests

;;

;;; Code:
(defpackage :math/tests
  (:use :cl :rt :math :std))

(in-package :math/tests)

(defsuite :math)
(in-suite :math)

(deftest sanity ()
  (signals math-error (error 'math-error))
  (signals math-warning (math-warning))
  (is (math::~ 0 0))
  (is= 4096 (length (hilbert-list 8)))
  (is (stringp (with-output-to-string (*standard-output*)
                 (cellular-automata)))))
(deftest axpy ())
(deftest gem ())
(deftest ger ())
(deftest sum ())
(deftest trs ())
(deftest norm ())
#+lapack
(deftest lu ())
#+lapack
(deftest qr ())
#+lapack
(deftest eig ())
#+lapack
(deftest lstsq ())
#+lapack
(deftest schur ())
#+lapack
(deftest svd ())
#+lapack
(deftest syl ())

;; (define-tensor-method axpy-test (alpha (x dense-tensor :a) (y dense-tensor :a t))
;;   `(let ((alpha (t/coerce ,(field-type (cl x)) alpha)))
;;      (declare (type ,(field-type (cl x)) alpha))
;;      ,(recursive-append
;;        (when (blas-tensorp (cl x))
;;	 `(if-let ((strd (and (call-alien-p x (t/l1-lb ,(cl x))) (blas-copyablep x y))))
;;	    (t/blas-axpy! ,(cl x) alpha x (first strd) y (second strd))))
;;        `(t/axpy! ,(cl x) alpha x y))))
