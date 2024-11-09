(defsystem :syn
  :version "0.1.0"
  :maintainer "Richard Westhaver <richard.westhaver@gmail.com>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on (:std :obj :parse :tree-sitter :doc :cli)
  :serial t
  :components ((:file "pkg")
               (:module "gen"
                :components
                ((:file "pkg")
                 (:file "condition")
                 (:file "var")
                 (:file "util")
                 (:file "read")
                 (:module "lisp"
                  :components
                  ((:file "pkg")
                   (:file "cl")
                   (:file "el")
                   (:file "scm")))
                 (:module "c"
                  :components ((:file "pkg")))
                 (:module "cu"
                  :components ((:file "pkg")))
                 (:module "rs"
                  :components ((:file "pkg")))
                 (:module "cpp"
                  :components ((:file "pkg")))
                 (:module "zig"
                  :components ((:file "pkg")))
                 (:module "py"
                  :components ((:file "pkg")))
                 (:module "js"
                  :components ((:file "pkg"))))))
  :in-order-to ((test-op (test-op "syn/tests"))))

(defsystem :syn/tests
  :depends-on (:rt :syn)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :syn)))
