;; (push :inspect *features*)
(defsystem :skel
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on (:uiop :asdf :sb-posix :sb-bsd-sockets :sb-concurrency :cl-ppcre :std
                     :obj :dat :cli :organ :doc :vc
                     :pod :net :box
                     (:feature :ext :krypt)
                     (:feature :ext :packy)
                     (:feature :gui :clouseau))
  :serial t
  :components 
  ((:module "core"
    :serial t
    :components 
    ((:file "pkg")
     (:file "condition")
     (:file "proto")
     (:file "header")
     (:file "vars")
     (:file "obj")
     (:file "print")
     (:file "util")
     (:file "vm")))
   (:module "comp"
    :components
    ((:file "pkg")
     (:file "asd")
     (:file "cargo")
     (:file "container")
     (:file "org")
     (:file "dir-locals")
     (:file "makefile")))
   (:module "net"
    :components
    ((:file "pkg")
     (:file "server")
     (:file "client")))
   (:file "pkg")
   (:module "tools"
    :components
    ((:file "pkg")
     (:file "deploy" :if-feature :deploy)
     (:file "viz" :if-feature :gui)))
   (:module "ext"
    :components
    ((:file "pkg")
     (:file "asdf")
     (:file "inspect" :if-feature :gui)
     (:file "krypt")
     (:file "packy"))
    :if-feature :ext))
  :in-order-to ((test-op (test-op "skel/tests"))))

(defsystem :skel/tests
  :depends-on (:rt :skel)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :skel)))
