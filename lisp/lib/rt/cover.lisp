;;; lib/rt/cover.lisp --- Test Coverage

;; wraps SB-COVER package

;;; Code:
(in-package :rt)

(defmacro enable-coverage ()
  `(declaim (optimize store-coverage-data)))

(defun disable-coverage ()
  `(declaim (optimize (sb-cover:store-coverage-data 0))))

(defmacro with-coverage (&body body)
  `(progn
     (enable-coverage)
     ,@body
     (disable-coverage)))

(defun coverage-report ()
  "Generate a coverage report."
  (sb-cover:report *coverage-directory*))
