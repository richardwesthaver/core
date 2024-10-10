;;; tests.lisp --- Apache Arrow FFI Tests

;; 

;;; Code:
(defpackage :arrow/tests
  (:use :cl :std :sb-alien :rt :arrow))
(in-package :arrow/tests)
(defsuite :arrow)
(in-suite :arrow)
(load-arrow)

(deftest sanity ()
  (is (= 1 arrow::+dlpack-major-version+))
  (is (zerop arrow::+dlpack-minor-version+))
  (is (= (alien-size arrow::arrow-schema) 576))
  (is (alien-size arrow::arrow-array) 640)
  (is (= (alien-size arrow::dl-tensor) 384)))
