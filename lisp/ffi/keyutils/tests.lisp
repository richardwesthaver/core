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
    (is= session-id (keyctl-get-keyring-id session-id 0))
    (iszero (keyctl-clear session-id))
    (let ((key-id (add-key "user" "mail" (make-alien-string "test" :null-terminate t) 5 session-id)))
      (is= (request-key "user" "mail" "payload" 0) key-id)
      (let ((pload (make-alien char 5)))
        (keyctl-read key-id pload 5)
        (isequal "test" (sb-ext:octets-to-string (clone-octets-from-alien pload (make-octets 4))))
        ;; TODO 2024-12-25: 
        (let ((desc (make-alien char 29)))
          (keyctl-describe key-id desc 29)
          (iszero (keyctl-set-timeout key-id 1))
          (istype 'string (sb-ext:octets-to-string (clone-octets-from-alien desc (make-octets 28)))))))))

(deftest persistent-keyring ()
  "See https://man7.org/linux/man-pages/man7/persistent-keyring.7.html."
  (let ((persist-id (keyctl-get-persistent (sb-posix:getuid) (key-spec :user))))

    (iszero (keyctl-unlink persist-id (key-spec :user)))))

(deftest thread-key ())
