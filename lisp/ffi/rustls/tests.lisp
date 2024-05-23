;;; rustls/tests.lisp --- librustls tests

;;; Code:
(defpackage :rustls/tests
  (:use :cl :std :rt :rustls))

(in-package :rustls/tests)

(defsuite :rustls)
(in-suite :rustls)

(load-rustls)

(deftest rustls ())

