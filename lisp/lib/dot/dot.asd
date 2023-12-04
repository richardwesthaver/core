(defsystem dot
  :version "0.1.0"
  :description "GraphViz dot compiler"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:std/all :cl-ppcre :nlp :dot/pkg)
  :in-order-to ((test-op (test-op :dot/tests)))
  :perform (test-op (o c) (symbol-call :rt :do-tests :dot)))
