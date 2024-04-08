;;; aud/tests.lisp --- Audio Tests

;;

;;; Code:
(defpackage :aud/tests
  (:use :cl :std :rt :log :aud))
(in-package :aud/tests)
(defsuite :aud)
(in-suite :aud)

(deftest sanity ())
