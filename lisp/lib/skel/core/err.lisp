;;; Conditions
(defpackage :skel/core/err
  (:use :cl :std/err :std/sxp))

(in-package :skel/core/err)

(define-condition skel-syntax-error (sxp-syntax-error) ())

(define-condition skel-fmt-error (sxp-fmt-error) ())
