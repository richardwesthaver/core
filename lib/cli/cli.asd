;;; cli.asd --- CLI library
(defsystem :cli
  :depends-on (:std :log :dat :obj)
  :components ((:file "pkg")
               (:file "ansi" :depends-on ("pkg"))
               (:file "env" :depends-on ("pkg"))
               (:file "progress" :depends-on ("pkg"))
               (:file "spark" :depends-on ("pkg"))
               (:file "terminfo" :depends-on ("pkg"))
               (:file "linedit" :depends-on ("pkg" "terminfo"))
               (:file "repl" :depends-on ("pkg"))
               (:file "shell" :depends-on ("env"))
               (:file "ed" :depends-on ("env"))
               (:module
                "tools"
                :components
                ((:file "pkg")
                 (:file "term" :depends-on ("pkg"))
                 (:file "tmux" :depends-on ("term"))
                 (:file "pacman" :depends-on ("pkg"))
                 (:file "cc" :depends-on ("pkg"))
                 (:file "build" :depends-on ("pkg"))
                 (:file "virt" :depends-on ("pkg"))
                 (:file "sys" :depends-on ("pkg"))
                 (:file "rust" :depends-on ("pkg"))
                 (:file "sbcl" :depends-on ("pkg"))
                 (:file "net" :depends-on ("pkg"))
                 (:file "media" :depends-on ("pkg"))
                 (:file "mail" :depends-on ("pkg"))
                 (:file "fs" :depends-on ("pkg"))
                 (:file "plot" :depends-on ("pkg"))))
               (:module "clap"
                :components
                ((:file "pkg")
                 (:file "ast" :depends-on ("pkg"))
                 (:file "vars" :depends-on ("pkg"))
                 (:file "util" :depends-on ("vars"))
                 (:file "macs" :depends-on ("util"))
                 (:file "proto" :depends-on ("util"))
                 (:file "opt" :depends-on ("macs" "proto" "ast"))
                 (:file "cmd" :depends-on ("macs" "proto" "ast"))
                 (:file "cli" :depends-on ("opt" "cmd"))))
               (:file "multi" :depends-on ("repl" "clap"))
               (:file "tui" :depends-on ("ansi" "progress" "spark" "linedit"))
               (:file "cli" :depends-on ("pkg" "clap")))
  :in-order-to ((test-op (test-op "cli/tests"))))

(defsystem :cli/tests
  :depends-on (:rt :cli)
  :components ((:module "tests"
                :components
                ((:file "pkg")
                 (:file "shell")
                 (:file "ansi")
                 (:file "clap"))))
  :perform (test-op (o c) (symbol-call :rt :do-tests :cli)))
