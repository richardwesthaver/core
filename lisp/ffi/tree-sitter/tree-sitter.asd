;;; tree-sitter.asd --- TREE-SITTER SYSTEMS

;; TREE-SITTER for lisp.

;;; Code:
(defsystem :tree-sitter
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:std :tree-sitter/pkg)
  :in-order-to ((test-op (test-op :tree-sitter/tests)))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :tree-sitter)))
