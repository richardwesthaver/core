;;; alpm/tests.lisp --- alpm tests

;;; Code:
(defpackage :alpm/tests
  (:use :cl :std :std/rt :alpm))

(in-package :alpm/tests)

(defsuite :alpm)
(in-suite :alpm)

(load-alpm)
