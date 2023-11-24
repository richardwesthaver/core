(defpackage :nlp/tests
  (:use :cl :std :std/rt :nlp/stem/porter))

(in-package :nlp/tests)

(defsuite :nlp)
(in-suite :nlp)

(deftest stem ()
  (stem "hacking"))
