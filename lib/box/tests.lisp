;;; box/tests.lisp --- Box tests

;;

;;; Code:
(defpackage :box/tests
  (:use :cl :rt :box :box/archiso :sb-bsd-sockets)
  (:export
   #:*archiso-json*))

(in-package :box/tests)
(defsuite :box)
(in-suite :box)

(defparameter *archiso-json*
  (with-open-file (file (asdf:system-relative-pathname :box #P"test.json"))
    (dat/json:json-read file)))

(deftest archiso ()
  (is *archiso-json*))
