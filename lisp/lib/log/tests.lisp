(defpackage :log/tests
  (:use :cl :std :rt :log))

(in-package :log/tests)

(defsuite :log)
(in-suite :log)

(deftest log ()
  "Test logging features"
  (is (debug! "test" *log-level*)))
