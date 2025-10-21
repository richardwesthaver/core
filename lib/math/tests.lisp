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
