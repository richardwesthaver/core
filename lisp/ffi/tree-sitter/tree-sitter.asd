;;; tree-sitter.asd --- TREE-SITTER SYSTEMS

;; TREE-SITTER for lisp.

;;; Code:
(defsystem "tree-sitter"
  :depends-on (:std)
  :in-order-to ((test-op (test-op "tree-sitter/tests")))
  :components ((:file "tree-sitter")))

(defsystem "tree-sitter/tests"
  :depends-on (:tree-sitter :rt)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :tree-sitter)))
