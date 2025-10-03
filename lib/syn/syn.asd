(defsystem :syn
  :version "0.1.0"
  :maintainer "Richard Westhaver <richard.westhaver@gmail.com>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on (:std :obj :parse :tree-sitter :doc :cli :io :dat)
  :serial t
  :components ((:file "pkg")
               (:file "ts")
               (:module "lang"
                :components
                ((:file "c")
                 (:file "js")
                 (:file "rs")
                 (:file "py")))
               (:module "gen"
                :components
                ((:file "condition")
                 (:file "var")
                 (:file "proto")
                 (:file "util")
                 (:file "read")
                 (:module "c"
                  :components 
                  ((:file "pkg")
                   (:file "read")
                   (:file "ast")
                   (:file "sym")
                   (:file "print")))
                 (:module "cpp"
                  :components 
                  ((:file "pkg")
                   (:file "sym")))
                 (:module "cu"
                  :components ((:file "pkg")))
                 (:module "rs"
                  :components 
                  ((:file "pkg")
                   (:file "read")
                   (:file "ast")
                   (:file "sym")
                   (:file "util")
                   (:file "print")))
                 (:module "py"
                  :components ((:file "pkg")))
                 (:module "js"
                  :components 
                  ((:file "pkg")
                   (:file "read")
                   (:file "ast")
                   (:file "sym")
                   (:file "print")
                   (:file "macs")))))
               (:file "fmt")
               (:file "lint")
               (:file "tempo")
               (:file "grovel")
	       (:file "cli"))
  :in-order-to ((test-op (test-op "syn/tests"))))

(defsystem :syn/tests
  :depends-on (:rt :syn)
  :components ((:module "tests"
                :components 
                ((:file "pkg")
                 (:module "gen"
                  :components
                  ((:file "pkg")
                   (:file "c")
                   (:file "cpp")
                   (:file "cu")
                   (:file "rs")))
                 (:module "lang"
                  :components
                  ((:file "pkg")
                   (:file "c")
                   (:file "rs")
                   (:file "js")
                   (:file "py"))))))
  :perform (test-op (o c) (symbol-call :rt :do-tests :syn)))
