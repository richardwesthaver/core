;;; lib/rt/cover.lisp --- Test Coverage

;; wraps SB-COVER package

;;; Code:
(in-package :rt/cover)
(defvar *coverage-directory* #P"/tmp/rt/")

(defun start-coverage ()
  (progn
    (declaim (optimize sb-c:store-coverage-data))))

(defun stop-coverage ()
  (progn
    (declaim (optimize (sb-c:store-coverage-data 0)))))

(defmacro with-coverage (&body body)
  `(progn
     (start-coverage)
     ,@body
     (stop-coverage)))

(defun coverage-report ()
  "Generate a coverage report."
  (sb-cover:report *coverage-directory*))
