;;; tests.lisp --- RT Tests

;;; Code:
(defpackage :rt/tests
  (:use :cl :std :rt :rt/flamegraph :rt/tracing :rt/fuzz))

(in-package :rt/tests)

(defsuite :rt
  :policy '(optimize sb-cover:store-coverage-data debug)
  :fixtures (list
             (make-fixture :tmp :name :fx1)))

(in-suite :rt)

(defun %foo (input)
  (loop for x below input
        collect (loop for y in (%foo x)
                      collect (cons x y))))

(deftest rt (:profile t :persist t)
  (with-fixture (fx :tmp :directory "/tmp/")
    (istype 'tmp-fixture fx))
  (signals (error t) (test-form (make-instance 'test-result))))

(deftest flamegraph (:cover t)
  (let ((f "/tmp/test.txt")) ;; open with https://speedscope.app or
                             ;; output svg with flamegraph.pl >>
                             ;; test.svg
    (with-flamegraph (f :sample-interval 0.00001 :show-progress t :report :flat)
      (%foo 20))
    (is (probe-file f))
    (delete-file f)))

(deftest tracing (:profile t)
  (let ((f "/tmp/tracing.json")
        (tracing::*default-arg-converter* tracing::+arg-converter-store-only-simple-objects-and-strings+)) ;; open with chrome://tracing
    (trace "STD")
    (with-tracing ("RT" "RT/TESTS")
      (%foo 25))
    (save-report f)
    (is (probe-file f))
    (delete-file f)))

(deftest fixture (:use (fx :tmp :directory "/tmp/fx1"))
  (is fx))

(deftest tmp ()
  (is (null (with-tmp-directory ())))
  (is (null (with-tmp-file (file))))
  (is (with-tmp-file (f1 :name "temporary-file")
        (is (probe-file *tmp*))
        (write-string "1 2 3 4" f1)
        (force-output f1)
        (is (= 7 (file-length f1)))))
  (is (with-tmp-directory ("foobar")
        (is (directory-path-p (probe-file *tmp*))))))

(deftest fuzz ()
  (defclass foo-fuzz (fuzzer) ())
  (is (integerp
       (fuzz (make-instance 'foo-fuzz))))
  (is (= 100 (length (fuzz* (make-random-state) (fuzz-generator (make-instance 'foo-fuzz)) :count 100)))))
