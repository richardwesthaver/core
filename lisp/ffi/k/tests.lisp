;;; k/tests.lisp --- k tests

;;; Code:
(defpackage :k/tests
  (:use :cl :std :std/rt :std/fu :k :std/alien :sb-ext))

(in-package :k/tests)

(defsuite :k)
(in-suite :k)

(load-k)

(deftest k ()
  (kinit))
