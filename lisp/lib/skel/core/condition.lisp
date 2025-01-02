;;; skel/core/condition.lisp --- Skel Errors

;;; Code:
(in-package :skel/core/condition)

(define-condition skel-condition () ()
  (:documentation "Superclass of SKEL error types."))

(eval-always
  (deferror skel-error (error skel-condition) ())
  (deferror skel-syntax-error (syntax-error skel-error) ())
  (deferror skel-io-error (skel-error) () (:auto t))
  (deferror skel-compile-error (skel-error) () (:auto t))
  (deferror skel-simple-error (simple-error skel-error) ()))

(defun skel-simple-error (fmt &rest args)
  (error 'skel-simple-error :format-control fmt :format-arguments args))

(deferror invalid-skel-ast (skel-syntax-error)
    ((ast :initform nil :initarg :ast :accessor ast))
    (:report (lambda (c s)
               (format s "Invalid Skel AST: ~A" (ast c)))))

(defun invalid-skel-ast (ast)
  (error 'invalid-skel-ast :ast ast))
