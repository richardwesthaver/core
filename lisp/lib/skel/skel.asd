(defsystem :skel
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on (:uiop :asdf :sb-posix :sb-bsd-sockets :sb-concurrency :cl-ppcre :std :obj :dat :cli :organ :doc)
  :serial t
  :components 
  ((:file "pkg")
   (:module "core"
    :serial t
    :components 
    ((:file "err")
     (:file "proto")
     (:file "header")
     (:file "virt")
     (:file "obj")
     (:module "vc"
      :serial t
      :components
      ((:file "proto")
       (:file "git")
       (:file "hg")))
     (:file "util")
     (:file "vm")))
   (:module "comp"
    :components
    ((:file "asd")
     (:file "containerfile")
     (:file "ignore")
     (:file "makefile")
     (:file "pkgbuild")))
   (:module "tools"
    :components
    ((:file "deploy")
     (:file "viz"))))
  :in-order-to ((test-op (test-op "skel/tests"))))

(defsystem :skel/tests
  :depends-on (:rt :skel)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :skel)))
