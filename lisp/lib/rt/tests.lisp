(defpackage :rt/tests
  (:use :cl :std :rt :sb-sprof :rt/flamegraph :rt/tracing :rt/cover :rt/bench))

(in-package :rt/tests)

(defsuite :rt)
(in-suite :rt)

(deftest rt (:profile t :persist t)
  (with-fixture (fx (:tmp :directory "/tmp/"))
    (is fx))
  (signals (error t) (test-form (make-instance 'test-result))))

(deftest flamegraph (:profile t)
  (let ((f "/tmp/test.txt")) ;; open with https://speedscope.app or
                             ;; output svg with flamegraph.pl >>
                             ;; test.svg
    (save-flamegraph (f :sample-interval 0.001 :show-progress t :report :flat)
      (loop for x from 0 to 1000
            do (* x x)))
    (is (probe-file f))
    (delete-file f)))

(deftest tracing (:profile t :skip t) ;; fails in x 
  (let ((f "/tmp/tracing.json")
        (*default-arg-converter* +arg-converter-store-only-simple-objects-and-strings+)) ;; open with chrome://tracing
    (flet ((foo (i)
             (let ((v0 (make-bit-vector 256))
                   (v1 (make-bit-vector 256 1)))
               (loop for x across v0
                     for y across v1
                     collect (list y (+ i x))))))
      (trace "STD")
      (start-tracing)
      (dotimes (i 100) (foo i))
      (save-report f))
    (is (probe-file f))
    (delete-file f)))

(deftest cover (:profile t)
  (start-coverage)
  (stop-coverage)
  (coverage-report))

(deftest tmp ()
  (is (null (with-tmp-directory ())))
  (is (null (with-tmp-file ())))
  (is (with-tmp-file (f1 :name "temporary-file")
        (is (probe-file *tmp*))
        (write-string "1 2 3 4" f1)
        (force-output f1)
        (is (= 7 (file-length f1)))))
  (is (with-tmp-directory ("foobar")
        (is (directory-path-p (probe-file *tmp*))))))
   
