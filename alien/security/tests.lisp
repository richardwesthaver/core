;;; security/tests.lisp --- Linux security tests

;;; Code:
(defpackage :security/tests
  (:use :cl :std :rt :security :sb-alien))

(in-package :security/tests)

(defsuite :security)
(in-suite :security)

(deftest pam-sanity ()
  (is (positive-integer-p linux-pam))
  (is (positive-integer-p linux-pam-minor)))
