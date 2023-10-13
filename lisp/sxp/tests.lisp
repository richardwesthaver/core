;;; tests.lisp --- SEXP tests
;; TODO 2023-09-01: refactor to macs.rt
(defpackage :sxp-tests
  (:use :cl :sxp :macs :rt)
  (:export #:run-tests))
(in-package :sxp-tests)
(in-readtable *macs-readtable*)
(declaim
 (type (or string pathname) *test-file*)
 (type string *test-string*))
(defparameter *test-file* "tests.sxp")
(defparameter *test-string* "(FOO 'BAR `(\"test\" ,BAZ ,@QUX) 123 0.0123 1/3 `(,A1 ,A2))")

(defsuite :sxp)
(in-suite :sxp)

(deftest forms ()
  (is (formp nil))
  (is (formp t))
  (is (formp 3.14))
  (is (formp "string"))
  (is (formp (mapc #`(',a1) '(a))))
  (is (formp ())))

(deftest sxp-file ()
  (let ((f (read-sxp-file *test-file*)))
    (is (equal (unwrap f) (unwrap f)))))

(deftest sxp-string ()
  (let ((f (make-instance 'sxp)))
    (is (formp (read-sxp-string f *test-string*)))
    (is (equalp (read-from-string (write-sxp-string f)) (read-from-string *test-string*)))))

(deftest sxp-stream ()
  (let ((f (make-instance 'sxp)))
    (with-input-from-string (s *test-string*)
      (read-sxp-stream f s))
    (is (write-sxp-stream f nil))))
