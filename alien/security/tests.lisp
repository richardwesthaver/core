;;; security/tests.lisp --- Linux security tests

;;; Code:
(defpackage :security/tests
  (:use :cl :std :rt :security :sb-alien))

(in-package :security/tests)

(defsuite :security)
(in-suite :security)
(load-pam)
(deftest pam-sanity ()
  (is (positive-integer-p linux-pam))
  (is (positive-integer-p linux-pam-minor)))

(deftest pam-simple ()
  (iseq :success
        (with-pam (s c e "")
          (security::pam-close-session (deref s) (pam-flags)))))

(deftest pam-creds ()
  (with-pam (s e "runuser")
    ;; calls default-conv
    (setf e (security::pam-authenticate (deref s) (pam-flags)))
    (security::pam-open-session (deref s) (pam-flags))
    (security::pam-setcred (deref s) (pam-flags))
    (security::pam-chauthtok (deref s) (pam-flags))))
