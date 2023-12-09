;;; alpm/tests.lisp --- alpm tests

;;; Code:
(defpackage :alpm/tests
  (:use :cl :std :std/rt :alpm))

(in-package :alpm/tests)

(defsuite :alpm)
(in-suite :alpm)

(load-alpm)

(deftest alpm-version ()
  (is (equal (alpm-version) "13.0.2")))
