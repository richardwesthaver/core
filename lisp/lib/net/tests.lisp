(defpackage :net/tests
  (:use :rt :std :cl :net))

(in-package :net/tests)

(defsuite :net)
(in-suite :net)

(deftest dns ())

(deftest tcp ())

(deftest udp ())

(deftest tlv ())

(deftest osc ())

(deftest sanity ())
