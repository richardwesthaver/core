;;; skel/core/condition.lisp --- Skel Errors

;;; Code:
(in-package :skel/core)

(define-condition skel-condition () ()
  (:documentation "Superclass of SKEL error types."))

(eval-always
  (deferror skel-error (error skel-condition) ())
  (deferror skel-syntax-error (syntax-error skel-error) ())
  (deferror skel-io-error (skel-error file-error) () (:reporter t))
  (deferror skel-compile-error (skel-error) () (:reporter t))
  (deferror skel-simple-error (simple-error skel-error) ()))

(defun skel-simple-error (fmt &rest args)
  (error 'skel-simple-error :format-control fmt :format-arguments args))

(deferror invalid-skel-bind (invalid-argument skel-compile-error) () (:reporter t))
