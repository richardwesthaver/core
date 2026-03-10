;;; syslog/tests.lisp --- syslog tests

;;; Code:
(defpackage :sys/tests
  (:use :cl :std :rt :sys :sb-alien))

(in-package :sys/tests)

(defsuite :sys)
(in-suite :sys)

(deftest syslog-sanity ()
  (openlog "foo" 0 0)
  (setlogmask 2)
  (syslog 0 "test message")
  (isnt (closelog)))
