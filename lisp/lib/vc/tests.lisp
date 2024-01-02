(defpackage :vc/tests
  (:use :cl :rt :vc))

(in-package :vc/tests)
(defsuite :vc)
(in-suite :vc)

(deftest git ())

(deftest hg ())
