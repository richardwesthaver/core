;;; cli.asd --- CLI library
(defsystem :cli
  :depends-on (:std :log #+readline :cl-readline)
  :components ((:file "pkg")
               (:file "ansi" :depends-on ("pkg"))
               (:file "env" :depends-on ("pkg"))
               (:file "shell" :depends-on ("env"))
               (:file "clap" :depends-on ("pkg" "shell" "ansi"))
               (:file "progress" :depends-on ("pkg"))
               (:file "spark" :depends-on ("pkg"))
               (:file "repl" :depends-on ("pkg"))
               (:file "prompt" :depends-on ("pkg" "env" "ansi"))
               (:file "ed" :depends-on ("pkg" "env")))
  :in-order-to ((test-op (test-op "cli/tests"))))

(defsystem :cli/tests
  :depends-on (:rt :cli)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :cli)))
