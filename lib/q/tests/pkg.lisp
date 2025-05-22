;;; pkg.lisp

;; Q Test Packages

;;; Code:
(defpackage :q/tests/fuzz
  (:use :cl :std :rt/fuzz :q :log :plan :schema :query))

(defpackage :q/tests
  (:use :cl :std :rt :q :log :parse/pratt :query :ast :plan :schema))

(in-package :q/tests)
(defsuite :q)
(in-suite :q)
;;; Query
(defclass bogus-data-source (data-source) ((db :initform nil :initarg :db)))

(defvar *basic-query* "SELECT * FROM employee WHERE state = 'CT'")

(deftest query-basic ()
  "Test the simple query `SELECT * FROM employee WHERE state = 'CT'` by manually
building a query-plan."
  (make-query *basic-query*))

(deftest sanity ()
  (is (make-instance 'query-engine
        :parser (make-instance 'query-parser)
        :optimizer (make-instance 'sql-optimizer)
        :sources nil)))
