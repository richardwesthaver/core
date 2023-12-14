(defpackage :nlp/tests
  (:use :cl :std :rt :nlp))

(in-package :nlp/tests)

(defsuite :nlp)
(in-suite :nlp)

(defvar %docs (make-instance 'document-collection))

(deftest porter-stem ()
  (is (string= (stem "hacking") "hack")))

(deftest dbscan ())

(deftest textrank ())

