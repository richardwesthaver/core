;;; bqn.asd --- BQN SYSTEMS

;; BQN for lisp.

;;; Commentary:

;; you must build CBQN with 'make shared-o3' and place the resulting
;; .so in a known path.

;; see also: https://github.com/Detegr/cbqn-sys

;;; Code:
(defsystem "bqn"
  :description "CBQN FFI"
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on (:std :bqn/pkg)
  :in-order-to ((test-op (test-op "bqn/tests")))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :bqn)))
