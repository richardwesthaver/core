;;; std/tests/sxp.lisp --- SXP tests
(defpackage :std/tests/sxp
    (:use :cl :sxp :std/base :std/fu :std/rt)
  (:export :*sxp-test-file* :*sxp-test-string*))

(in-package :std/tests/sxp)
(in-readtable :std)
(declaim
 (type (or string pathname) *sxp-test-file*)
 (type string *sxp-test-string*))
(defparameter *sxp-test-file* "tests.sxp")
(defparameter *sxp-test-string* "(FOO 'BAR `(\"test\" ,BAZ ,@QUX) 123 0.0123 1/3 `(,A1 ,A2))")

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
  (let ((f (read-sxp-file *sxp-test-file*)))
    (is (equal (unwrap f) (unwrap f)))))

(deftest sxp-string ()
  (let ((f (make-instance 'sxp)))
    (is (formp (read-sxp-string f *sxp-test-string*)))
    (is (equalp (read-from-string (write-sxp-string f)) (read-from-string *sxp-test-string*)))))

(deftest sxp-stream ()
  (let ((f (make-instance 'sxp)))
    (with-input-from-string (s *sxp-test-string*)
      (read-sxp-stream f s))
    (with-output-to-string (s)
      (is (write-sxp-stream f s)))))
