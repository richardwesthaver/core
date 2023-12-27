;;; cli.asd --- CLI library
(defsystem :cli
  :depends-on (:std :log)
  :components ((:file "pkg")
               (:file "api" :depends-on ("pkg"))
               (:file "progress" :depends-on ("pkg"))
               (:file "spark" :depends-on ("pkg"))
               (:file "repl" :depends-on ("pkg"))
               (:file "prompt" :depends-on ("pkg"))
               (:file "ed" :depends-on ("pkg")))
  :in-order-to ((test-op (test-op "cli/tests"))))

(defsystem :cli/tests
  :depends-on (:rt :cli)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :cli)))
