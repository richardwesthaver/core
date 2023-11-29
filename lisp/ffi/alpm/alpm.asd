;;; alpm.asd --- ALPM SYSTEMS

;; ALPM for lisp.

;;; Commentary:

;;; Code:
(defsystem "alpm"
  :description "ALPM FFI"
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on (:std :alpm/pkg)
  :in-order-to ((test-op (test-op "alpm/tests")))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :alpm)))
