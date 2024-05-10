;;; Conditions
(in-package :skel/core)

(define-condition skel-error (std-error) ())

(deferror skel-syntax-error (sxp-syntax-error) () (:auto t))
(deferror skel-fmt-error (sxp-fmt-error) () (:auto t))
(deferror skel-compile-error (skel-error) () (:auto t))
