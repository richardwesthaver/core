(defpackage :cry/tests
  (:use :rt :std :cl :cry :cry/hotp :cry/totp :cry/crc64 :cry/jwt :cry/b3))

(in-package :cry/tests)

(defsuite :cry)
(in-suite :cry)

(deftest hotp ()
  (is (integerp (hotp "1234" 100))))
(deftest totp ()
  (is (integerp (totp "1234"))))
(deftest crc64 ()
  (init-crc64 42)
  (is (integerp (crc64-sequence "aaaaaaaaaaaaaaaaaaaaaaa"))))
(deftest b3 ()
  (blake3:load-blake3)
  (isequal
   (b3hash-string "1234")
   (b3hash-string "1234")))
(deftest jwt ()
  ;; https://jwt.io/#debugger-io
  (multiple-value-bind (claims header)
      (cry/jwt:jwt-decode "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c" :secret "your-256-bit-secret")
    (istype 'dat/json:json-object claims)
    (istype 'dat/json:json-object header)))
    

