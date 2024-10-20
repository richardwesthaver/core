(defsystem :syn
  :version "0.1.0"
  :maintainer "Richard Westhaver <richard.westhaver@gmail.com>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on (:std :obj :parse :tree-sitter)
  :serial t
  :components ((:file "pkg")
               (:module "gen"
                :components
                ((:file "pkg")
                 (:file "read")
                 (:file "el")
                 (:file "scm")
                 (:file "c")
                 (:file "cu")
                 (:file "rs")
                 (:file "cpp")
                 (:file "zig")
                 (:file "py")
                 (:file "js"))))
  :in-order-to ((test-op (test-op "syn/tests"))))

(defsystem :syn/tests
  :depends-on (:rt :syn)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :syn)))
