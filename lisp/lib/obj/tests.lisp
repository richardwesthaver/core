(defpackage :obj/tests
  (:use :cl :std :rt :obj/id :obj/color :obj/tbl))

(in-package :obj/tests)

(defsuite :obj)
(in-suite :obj)

(deftest rainbow ())

(deftest tables ())

(deftest ids ()
  (is (= (reset-id t) (reset-id '(1 2 3))))
  (is (not (equalp (make-id) (make-id)))))

(deftest def-iter ())

(deftest def-seq ())
