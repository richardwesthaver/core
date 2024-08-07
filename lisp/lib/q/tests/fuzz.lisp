;;; fuzz.lisp --- Q Fuzzers

;; Q Test Fuzzers

;;; Code:
(in-package :q/tests/fuzz)

(defclass query-fuzzer (fuzzer) (data-source))

(defclass sql-fuzzer (query-fuzzer) ())

(defclass dql-fuzzer (query-fuzzer) ())
