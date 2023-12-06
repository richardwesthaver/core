;;; Conditions
(defpackage :skel/core/err
  (:use :cl :std/err :std/sxp)
  (:export 
   :skel-syntax-error :skel-fmt-error
   :skel-compile-error))

(in-package :skel/core/err)

(define-condition skel-syntax-error (sxp-syntax-error) ())
(define-condition skel-fmt-error (sxp-fmt-error) ())
(define-condition skel-compile-error nil nil)
