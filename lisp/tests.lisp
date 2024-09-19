;;; tests.lisp --- Test Package Definitions

;; 

;;; Code:
(in-package :std-user)

(defpkg :core/tests
  (:use :std-lisp :rt :log))

(in-package :core/tests)

(defsuite :core)
(in-suite :core)

(defun run-all-tests (&optional force)
  (mapcar (lambda (x) (do-tests x force)) (remove *test-suite* *test-suite-list*)))

(deftest all ()
  (do-tests *test-suite-list*
