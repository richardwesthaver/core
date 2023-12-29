;;; ssh2/tests.lisp --- libssh2 tests

;;; Code:
(defpackage :ssh2/tests
  (:use :cl :std :rt :ssh2))

(in-package :ssh2/tests)

(defsuite :ssh2)
(in-suite :ssh2)

(load-ssh2)

(deftest ssh2 ())

