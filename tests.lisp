;;; tests.lisp --- Test Package Definitions

;; 

;;; Code:
(pkg:defpkg :core/tests
  (:use :std-lisp :rt :log)
  (:export :run-all-tests :core-coverage-report))

(in-package :core/tests)

(defsuite :core)
(in-suite :core)
