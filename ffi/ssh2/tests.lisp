;;; ssh2/tests.lisp --- libssh2 tests

;;; Code:
(defpackage :ssh2/tests
  (:use :cl :std :rt :ssh2 :sb-alien))

(in-package :ssh2/tests)

(defsuite :ssh2)
(in-suite :ssh2)

(load-ssh2)

(deftest sanity ()
  (iszero (libssh2-init 0))
  (isnt (libssh2-exit)))

(deftest session-init ()
  (with-alien ((arr (* c-string)))
    (let ((sesh (libssh2-session-init-ex nil nil nil nil)))
      (libssh2-session-supported-algs sesh 0 (addr arr))
      (is (find "curve25519-sha256" (c-strings-to-string-list arr) :test 'string=))
      (free-alien sesh))))
