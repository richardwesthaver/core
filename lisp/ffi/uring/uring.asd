;;; uring.asd-*- mode: lisp; -*-
(defsystem :uring
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:std :uring/pkg)
  :in-order-to ((test-op (test-op :uring/tests)))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :uring)))
