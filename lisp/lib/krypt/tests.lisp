;;; krypt/tests.lisp --- Krypt Tests

;;

;;; Code:
(defpackage :krypt/tests
  (:use :cl :std :rt :krypt))

(in-package :krypt/tests)

(defsuite :krypt)
(in-suite :krypt)

(deftest config ())
