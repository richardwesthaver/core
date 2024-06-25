;; (push :inspect *features*)
(defsystem :skel
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on (:uiop :asdf :sb-posix :sb-bsd-sockets :sb-concurrency :cl-ppcre :std
                     :obj :dat :cli :organ :doc :vc
                     (:feature :ext :pod)
                     (:feature :ext :box)
                     (:feature :ext :krypt)
                     (:feature :ext :packy)
                     (:feature :ext :net)
                     (:feature (:and :ext :clouseau) :clouseau))
  :serial t
  :components 
  ((:module "core"
    :serial t
    :components 
    ((:file "pkg")
     (:file "err")
     (:file "types")
     (:file "proto")
     (:file "header")
     (:file "vars")
     (:file "obj")
     (:file "util")
     (:file "vm")))
   (:module "comp"
    :components
    ((:file "pkg")
     (:file "asd")
     (:file "cargo")
     (:file "makefile")))
   (:file "pkg")
   (:module "tools"
    :components
    ((:file "pkg")
     (:file "deploy")
     (:file "viz"))
    :if-feature :tools)
   (:module "ext"
    :components
    ((:file "pkg")
     (:file "asdf")
     (:file "inspect" :if-feature :clouseau)
     (:file "krypt")
     (:file "packy")
     (:file "net"))
    :if-feature :ext))
  :in-order-to ((test-op (test-op "skel/tests"))))

(defsystem :skel/tests
  :depends-on (:rt :skel)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :skel)))
