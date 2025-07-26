;; (push :inspect *features*)
(defsystem :skel
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on (:std :obj :dat :cli :organ :doc :vc :pod :net :box :rdb :syn)
  :serial t
  :components 
  ((:module "core"
    :serial t
    :components 
    ((:file "pkg")
     (:file "condition")
     (:file "proto")
     (:file "header")
     (:file "var")
     (:file "obj")
     (:file "component")
     (:file "rule")
     (:file "project")
     (:file "print")
     (:file "fs")
     (:file "schema")
     (:file "db")
     (:file "log")
     (:file "util")
     (:file "vm")))
   (:module "comp"
    :components
    ((:file "pkg")
     (:file "lisp")
     (:file "asd")
     (:file "cargo")
     (:file "container")
     (:file "org")
     (:file "dir-locals")
     (:file "makefile")))
   (:file "pkg")
   (:file "srv")
   (:file "infer")
   (:module "net"
    :components
    ((:file "var")
     (:file "server")
     (:file "client")
     (:file "endpoint")))
   (:file "cli")
   (:file "skel"))
  :in-order-to ((test-op (test-op "skel/tests"))))

(defsystem :skel/tests
  :depends-on (:rt :skel)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :skel)))
