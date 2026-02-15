;;; syslog/tests.lisp --- syslog tests

;;; Code:
(defpackage :syslog/tests
  (:use :cl :std :rt :syslog :sb-alien))

(in-package :syslog/tests)

(defsuite :syslog)
(in-suite :syslog)

(deftest sanity ()
  (openlog "foo" 0 0)
  (setlogmask 2)
  (syslog 0 "test message")
  (isnt (closelog)))
