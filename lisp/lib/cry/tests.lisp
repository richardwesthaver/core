(defpackage :cry/tests
  (:use :rt :std :cl :cry :cry/hotp :cry/totp :cry/crc64 :cry/jwt))

(in-package :cry/tests)

(defsuite :cry)
(in-suite :cry)

(deftest hotp ()
  (is (integerp (hotp "1234" 0))))
(deftest totp ()
  (is (integerp (totp "1234"))))
(deftest crc64 ()
  (init-crc64 42)
  (is (integerp (crc64-sequence "aaaaaaaaaaaaaaaaaaaaaaa"))))
(deftest jwt (:skip t))

