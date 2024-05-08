;;; Conditions
(in-package :skel/core)

(define-condition skel-error (std-error) ())

(deferror skel-syntax-error (sxp-syntax-error) () (:auto t))
(define-condition skel-fmt-error (sxp-fmt-error) ())
(define-condition skel-compile-error nil nil)
