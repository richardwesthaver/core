(defpackage :obj/tests
  (:use :cl :std :rt :obj))

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

(deftest uris ()
  "Tests for different types of URIs. Attempts to conform with RFCs and test suites."
  (uri-host (parse-uri-string-rfc3986 "https://localhost"))
)
