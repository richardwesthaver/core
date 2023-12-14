;;; cli.asd --- CLI library
(defsystem :cli
  :depends-on (:std :log)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "std/tests"))))

(defsystem :cli/tests
  :depends-on (:rt :cli)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :cli)))
