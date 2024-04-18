(in-package :vc)

(define-condition vc-error (std-error) ())

(deferror git-error (vc-error) ())

(deferror hg-error (vc-error) ())
