;;; Conditions
(in-package :skel/core)
(eval-always
  (deferror skel-error (std-error) () (:auto t)))

(deferror skel-syntax-error (sxp-syntax-error) () (:auto t))
(deferror skel-fmt-error (sxp-fmt-error) () (:auto t))
(deferror skel-compile-error (skel-error) () (:auto t))
