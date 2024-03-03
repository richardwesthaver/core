;;; box/tests.lisp --- Box tests

;;

;;; Code:
(defpackage :box/tests
  (:use :cl :rt :box :sb-bsd-sockets))

(in-package :box/tests)
(defsuite :box)
(in-suite :box)
