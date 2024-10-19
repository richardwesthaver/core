;;; lib/rt/cover.lisp --- Test Coverage

;; wraps SB-COVER package

;;; Code:
(in-package :rt/cover)
(defvar *coverage-directory* #P"/tmp/rt/")

(defun enable-coverage ()
  (proclaim '(optimize sb-cover:store-coverage-data)))

(defun disable-coverage ()
  (proclaim '(optimize (sb-c:store-coverage-data 0))))

(defun coverage-report ()
  "Generate a coverage report."
  (sb-cover:report *coverage-directory*))
