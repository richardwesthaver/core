(defpackage :syn/tests
  (:use :cl :rt :syn :syn/ts))

(in-package :syn/tests)
(defsuite :syn)
(in-suite :syn)
(defsuite :syn/gen)
(in-suite :syn/gen)

(deftest sxp (:skip t))
(deftest file-headers (:skip t))

(deftest el ())
(deftest scm ())
(deftest c ())
(deftest rs ())
(deftest js ())
(deftest py ())
(deftest sh ())
(deftest zig ())
(deftest cu ())
(deftest cpp ())
