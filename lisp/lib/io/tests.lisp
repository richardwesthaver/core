(defpackage :io/tests
  (:use :cl :std :rt :io :uring))
(in-package :io/tests)
(defsuite :io)
(in-suite :io)

(load-uring)

(deftest sanity ()
  (uring::io-uring-major-version))
