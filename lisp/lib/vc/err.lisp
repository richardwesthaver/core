(in-package :vc)

(define-condition vc-error (std-error) ())

(deferror git-error (vc-error) () (:auto t))

(deferror hg-error (vc-error) () (:auto t))
