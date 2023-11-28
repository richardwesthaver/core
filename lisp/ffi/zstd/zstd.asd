;;; uring.asd-*- mode: lisp; -*-
(defsystem :zstd
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:std :zstd/pkg)
  :in-order-to ((test-op (test-op :zstd/tests)))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :zstd)))
