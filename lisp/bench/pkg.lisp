;;; bench.lisp --- Core Benchmarks

;; 

;;; Code:
(in-package :std-user)

(defpkg :core/bench
  (:use :std-lisp :rt :log :rt/bench))

(in-package :core/bench)

(defbench simple () "")

(defun core-coverage ()
  (cover:clear-coverage)
  (cover:with-coverage
    (asdf:load-system :core/tests)
    (rt:do-tests :core))
  (cover:coverage-report))
