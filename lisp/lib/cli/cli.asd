;;; cli.asd --- CLI library
(defsystem :cli
  :depends-on (:std :log)
  :components ((:file "pkg")
               (:file "ansi" :depends-on ("pkg"))
               (:file "env" :depends-on ("pkg"))
               (:file "progress" :depends-on ("pkg"))
               (:file "spark" :depends-on ("pkg"))
               (:file "repl" :depends-on ("pkg"))
               (:file "shell" :depends-on ("env"))
               (:file "prompt" :depends-on ("env" "ansi"))
               (:file "ed" :depends-on ("env"))
               (:module
                "tools"
                :components
                ((:file "tmux")
                 (:file "pacman")))
               (:file "clap" :depends-on ("shell" "prompt"))
               (:file "clap/pkg")
               (:file "clap/vars")
               (:file "clap/macs")
               (:file "clap/proto")
               (:file "clap/opt")
               (:file "clap/cmd")
               (:file "clap/cli"))
  :in-order-to ((test-op (test-op "cli/tests"))))

(defsystem :cli/tests
  :depends-on (:rt :cli)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :cli)))
