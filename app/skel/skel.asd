;; (push :inspect *features*)
(defsystem :skel
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on (:std :obj :dat :cli :organ :doc :vc :pod :net :box :rdb :syn (:feature :mpk :mpk))
  :serial t
  :components 
  ((:file "pkg")
   (:module "core"
    :serial t
    :components 
    ((:file "condition")
     (:file "proto")
     (:file "header")
     (:file "var")
     (:file "obj")
     (:file "component")
     (:file "rule")
     (:file "project")
     (:file "pack")
     (:file "print")
     (:file "schema")
     (:file "db")
     (:file "log")
     (:file "util")))
   (:module "comp"
    :components
    ((:file "lisp")
     (:file "asd")
     (:file "cargo")
     (:file "box")
     (:file "container")
     (:file "org")
     (:file "dir-locals")
     (:file "makefile")
     (:file "sys")
     (:file "infer")))
   (:module "packy"
    :components
    ((:file "pkg")
     (:file "var")
     (:file "proto")
     (:file "pkgbuild")
     (:file "apkbuild")
     (:file "db")
     (:file "client")
     (:file "srv")
     (:file "cfg")
     (:file "cli")))
   (:module "krypt"
    :components 
    ((:file "pkg")
     (:file "condition")
     (:file "krypt")
     (:file "cli")))
   (:module "homer"
    :components 
    ((:file "pkg")
     (:file "var")
     (:file "log")
     (:file "util")
     (:file "task")
     (:file "cfg")
     (:file "srv")
     (:file "cli")))
   (:module "net"
    :components
    ((:file "var")
     (:file "server")
     (:file "client")
     (:file "endpoint")
     (:file "srv")))
   (:file "cli")
   (:file "skel"))
  :in-order-to ((test-op (test-op "skel/tests"))))

(defsystem :skel/tests
  :depends-on (:rt :skel)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :skel)))
