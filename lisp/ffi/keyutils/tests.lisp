;;; keyutils/tests.lisp --- libkeyutils tests

;;; Code:
(defpackage :keyutils/tests
  (:use :cl :std :rt :keyutils :sb-alien))

(in-package :keyutils/tests)

(defsuite :keyutils)
(in-suite :keyutils)

(load-keyutils)

(deftest sanity ()
  (is (string= "keyutils" (car (ssplit #\- (cast keyutils-version-string c-string)))))
  (is (every 'minusp (list (key-spec :thread)
                             (key-spec :user)
                             (key-spec :user-session)
                             (key-spec :session)
                             (key-spec :group)
                             (key-spec :process)
                             (key-spec :thread)
                             (key-spec :reqkey-auth)))))

(deftest keyutils ()
  (let ((session-id (keyctl-join-session-keyring (symbol-name (gensym "test")))))
    (is (integerp (keyctl-get-keyring-id session-id 1)))))

(deftest process-key ())

(deftest thread-keys ())
