;;; readline/tests.lisp --- readline tests

;;; Code:
(defpackage :readline/tests
  (:use :cl :std :rt :readline))

(in-package :readline/tests)

(defsuite :readline)
(in-suite :readline)

(load-readline)

(deftest sanity ()
  (is readline::*history-base*)
  (is readline::*history-length*))
