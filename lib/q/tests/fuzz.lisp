;;; fuzz.lisp --- Q Fuzzers

;; Q Test Fuzzers

;;; Code:
(in-package :q/tests/fuzz)

(defvar *fuzz-value-max-size* 32)

;; > schema, state, generator
(defkernel query-fuzzer (fuzzer data-source) ())

(defun generate-sql-type (state &optional (type :string))
  (case type
    (:integer (make-instance 'sql-number :value (random most-positive-fixnum)))
    (:float (make-instance 'sql-number :value (random most-positive-single-float)))
    (:double (make-instance 'sql-number :value (random most-positive-double-float)))
    (:string (make-instance 'sql-string :value (random-chars (random *fuzz-value-max-size* state))))))

(defun generate-dql-type (state &optional (type :string)))

(defkernel sql-fuzzer (query-fuzzer) ()
  (:kernel #'generate-sql-type))

(defmethod fuzz ((self sql-fuzzer) &key type)
  (funcall (kernel self) (state self) type))

(defkernel dql-fuzzer (query-fuzzer) ()
  (:kernel #'generate-dql-type))
