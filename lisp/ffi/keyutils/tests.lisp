;;; keyutils/tests.lisp --- libkeyutils tests

;;; Code:
(defpackage :keyutils/tests
  (:use :cl :std :rt :keyutils))

(in-package :keyutils/tests)

(defsuite :keyutils)
(in-suite :keyutils)

(load-keyutils)

(deftest keyutils ())

