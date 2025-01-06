;;; tests.lisp --- Test Package Definitions

;; 

;;; Code:
(pkg:defpkg :core/tests
  (:use :std-lisp :rt :log)
  (:export :run-all-tests :core-coverage-report))

(in-package :core/tests)

(defsuite :core)
(in-suite :core)

(declaim (inline run-all-tests))
(defun run-all-tests (&optional force)
  (mapcar (lambda (x) (do-tests x force)) (remove *test-suite* *test-suite-list*)))

(defun core-coverage-report ()
  (sb-cover:clear-coverage)
  (rt/cover:enable-coverage)
  (sb-sprof:with-profiling (:report :graph :reset t 
                            :show-progress t)

    (asdf:compile-system :core :force t)
    (asdf:compile-system :core/tests :force t)
    (asdf:load-system :core/tests :force t)
    (run-all-tests t))
  (rt/cover:report "/tmp/rt/"))

;; (core-coverage-report)

(deftest all ()
  (run-all-tests))
