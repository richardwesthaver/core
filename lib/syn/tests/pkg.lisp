(defpackage :syn/tests
  (:use :cl :rt :syn :syn/ts))

(in-package :syn/tests)
(defsuite :syn)
(in-suite :syn)

(deftest sxp (:skip t))
(deftest file-headers (:skip t))
