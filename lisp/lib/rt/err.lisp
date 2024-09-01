;;; lib/rt/err.lisp --- RT Errors

;; Errors which may occur within the Regression Testing framework.

;;; Code:

(in-package :rt)

(define-condition test-condition () ())

(define-condition test-failed (test-condition error)
  ((reason :accessor fail-reason :initarg :reason :initform "unknown")
   (name :accessor fail-name :initarg :name)
   (form :accessor fail-form :initarg :form))
  (:documentation "Signaled when a test fails.")
  (:report (lambda (c s)
             (format s "The following expression failed: ~S~%~A."
                     (fail-form c)
                     (fail-reason c)))))
