;;; k/tests.lisp --- k tests

;;; Code:
(defpackage :bqn/tests
  (:use :cl :std :rt :bqn :sb-ext))

(in-package :bqn/tests)

(defsuite :bqn)
(in-suite :bqn)

(load-bqn)

(deftest bqn ()
  (bqn-init))
