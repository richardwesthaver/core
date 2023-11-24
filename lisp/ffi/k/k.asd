;;; k.asd --- K SYSTEMS

;; k for lisp.

;;; Commentary:

;; 

;;; Code:
(defsystem "k"
  :description "ngn/k FFI"
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on (:std :k/pkg)
  :in-order-to ((test-op (test-op "k/tests")))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :k)))
