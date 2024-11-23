;;; cli.asd --- CLI library
(defsystem :cli
  :depends-on (:std :log :dat :obj :readline)
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
                ((:file "pkg")
                 (:file "term" :depends-on ("pkg"))
                 (:file "tmux" :depends-on ("term"))
                 (:file "pacman" :depends-on ("pkg"))
                 (:file "cc" :depends-on ("pkg"))
                 (:file "nvcc" :depends-on ("pkg"))
                 (:file "systemd" :depends-on ("pkg"))
                 (:file "wg" :depends-on ("pkg"))
                 (:file "cargo" :depends-on ("pkg"))
                 (:file "sbcl" :depends-on ("pkg"))
                 (:file "ytdl" :depends-on ("pkg"))))
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
               (:file "cli"))
  :in-order-to ((test-op (test-op "cli/tests"))))

(defsystem :cli/tests
  :depends-on (:rt :cli)
  :components ((:module "tests"
                :components
                ((:file "pkg")
                 (:file "shell")
                 (:file "ansi")
                 (:file "clap")
                 (:file "tools"))))
  :perform (test-op (o c) (symbol-call :rt :do-tests :cli)))
