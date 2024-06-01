;;; box/tests.lisp --- Box tests

;;

;;; Code:
(defpackage :box/tests
  (:use :cl :rt :box :box/archiso :sb-bsd-sockets))

(in-package :box/tests)
(defsuite :box)
(in-suite :box)

(deftest archiso ()
  (is (with-open-file (file #P"test.json")
        (inspect (dat/json:json-read file)))))
