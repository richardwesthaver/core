;;; keyutils/tests.lisp --- libkeyutils tests

;;; Code:
(defpackage :keyutils/tests
  (:use :cl :std :rt :keyutils :sb-alien))

(in-package :keyutils/tests)

(defsuite :keyutils)
(in-suite :keyutils)

(load-keyutils)

(deftest sanity ()
  (is (string= "keyutils" (car (ssplit #\- (cast keyutils-version-string c-string))))))

(deftest keyutils ()
  (let ((session-id (keyctl-join-session-keyring (symbol-name (gensym "test")))))
    (is (integerp (keyctl-get-keyring-id session-id 1)))))
