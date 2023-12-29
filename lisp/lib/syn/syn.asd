(defsystem :syn
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on (:std :obj :parse :tree-sitter)
  :serial t
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "syn/tests"))))

(defsystem :syn/tests
  :depends-on (:rt :syn)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :syn)))
