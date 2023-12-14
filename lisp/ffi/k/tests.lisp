;;; k/tests.lisp --- k tests

;;; Code:
(defpackage :k/tests
  (:use :cl :std :rt :k :sb-ext))

(in-package :k/tests)

(defsuite :k)
(in-suite :k)

(load-k)

(deftest k ()
  (kinit))
