;;; openssl/tests.lisp --- AWS-LC tests

;;; Code:
(defpackage :openssl/tests
  (:use :cl :std :rt :openssl))

(in-package :openssl/tests)

(defsuite :openssl)
(in-suite :openssl)

(load-crypto)
(load-ssl)

(deftest sanity ())
