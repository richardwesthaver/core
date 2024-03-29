(defpackage :uring/tests 
    (:use :cl :rt :std :uring))
(in-package :uring/tests)
(defsuite :uring)
(in-suite :uring)
(load-uring)

(deftest sanity ()
  (is (= 1 (io-uring-check-version (io-uring-major-version) (io-uring-minor-version)))))
