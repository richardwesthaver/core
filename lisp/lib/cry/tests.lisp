(defpackage :cry/tests
  (:use :rt :std :cl :cry :cry/hotp :cry/totp :cry/crc64))

(in-package :cry/tests)

(defsuite :cry)
(in-suite :cry)

(deftest sanity ()
  (is (integerp (hotp "1234" 0)))
  (is (integerp (totp "1234")))
  (init-crc64 42)
  (is (integerp (crc64-sequence "aaaaaaaaaaaaaaaaaaaaaaa"))))
