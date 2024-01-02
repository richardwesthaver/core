(in-package :vc)

(define-condition vc-error (std-error) ())

(define-condition git-error (vc-error) ())

(define-condition hg-error (vc-error) ())
