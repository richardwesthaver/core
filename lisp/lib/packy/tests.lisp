(defpackage :packy/tests
  (:use :cl :rt :packy))

(in-package :packy/tests)
(defsuite :packy)
(in-suite :packy)

(deftest packy-objects ())
