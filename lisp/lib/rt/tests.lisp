(defpackage :rt/tests
  (:use :cl :std :rt :sb-sprof :rt/flamegraph :rt/tracing :rt/cover :rt/bench))

(in-package :rt/tests)

(defsuite :rt)
(in-suite :rt)

(deftest rt (:profile t :persist t)
  (is (typep (make-fixture-prototype :empty nil) 'fixture-prototype))
  (with-fixture (fx (make-fixture ((a 1) (b 2))
		      (:+ () (+ (incf a) (incf b)))
		      (:- () (- (decf a) (decf b)))
		      (t () 0)))
    (is (= 5 (funcall fx :+)))
    (is (= 7 (funcall fx :+)))
    (is (= -1 (funcall fx :-)))
    (is (= 0 (funcall fx))))
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
