(defpackage :rt/tests
  (:use :cl :std :rt))

(in-package :rt/tests)

(defsuite :rt)
(in-suite :rt)

(deftest rt (:bench 100 :profile t :persist nil)
  (is (typep (make-fixture-prototype :empty nil) 'fixture-prototype))
  (with-fixture (fx (make-fixture ((a 1) (b 2)) 
		      (:+ () (+ (incf a) (incf b)))
		      (:- () (- (decf a) (decf b)))
		      (t () 0)))
    (is (= 5 (funcall fx :+)))
    (is (= 7 (funcall fx :+)))
    (is (= 5 (funcall fx :-)))
    (is (= 0 (funcall fx))))
  (signals (error t) (test-form (make-instance 'test-result))))
