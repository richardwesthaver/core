;;; math/tests.lisp --- MATH tests

;;

;;; Code:
(defpackage :math/tests
  (:use :cl :rt :math :std))

(in-package :math/tests)

(defsuite :math)
(in-suite :math)

(deftest sanity ()
  (signals math-error (math-error))
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
