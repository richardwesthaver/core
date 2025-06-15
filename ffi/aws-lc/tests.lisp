;;; aws-lc/tests.lisp --- AWS-LC tests

;;; Code:
(defpackage :aws-lc/tests
  (:use :cl :std :rt :aws-lc))

(in-package :aws-lc/tests)

(defsuite :aws-lc)
(in-suite :aws-lc)

(load-crypto)
(load-ssl)

(deftest sanity ())
