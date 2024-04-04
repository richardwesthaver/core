(defpackage :io/tests
  (:use :cl :std :rt :io :uring))
(in-package :io/tests)
(defsuite :io)
(in-suite :io)

(load-uring)

(deftest sanity ()
  (uring::io-uring-major-version))

(deftest serve-event ()
  "See 'tests/serve-event.pure.lisp'."
  nil)
compute-pollfds
(make-handler :output 1030 #'car)
