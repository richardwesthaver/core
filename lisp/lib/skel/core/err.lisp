;;; Conditions
(in-package :skel/core)

(define-condition skel-error (std-error) ())

(define-condition skel-syntax-error (sxp-syntax-error) ())
(define-condition skel-fmt-error (sxp-fmt-error) ())
(define-condition skel-compile-error nil nil)
