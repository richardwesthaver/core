(defsystem :dot
  :version "0.1.0"
  :description "GraphViz dot compiler"
  :depends-on (:cl-ppcre :std :nlp)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op :dot/tests))))

(defsystem :dot/tests
  :depends-on (:rt :dot)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :dot)))
