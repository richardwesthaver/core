
(defpackage :doc/tests
  (:use :cl :rt :doc))

(in-package :doc/tests)

(defsuite :doc)
(in-suite :doc)

(deftest doc-symbol ())

(deftest doc-package ())

(deftest doc-system ())

(deftest doc-file ())
