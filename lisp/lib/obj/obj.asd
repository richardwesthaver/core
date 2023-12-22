(defsystem :obj
  :description "Lisp object library"
  :depends-on (:std :cli)
  :serial t
  :components ((:file "pkg")
               (:module "hash"
                :components ((:file "hasher")
                             (:file "map")
                             (:file "set")
                             (:file "chash")
                             (:file "castable")))
               (:file "id")
               (:file "seq")
               (:file "tree")
               (:module "graph"
                :components ((:file "pkg")))
               (:file "color")
               (:file "tbl")
               (:module "db"
                :components ((:file "mop")
                             (:file "proto")
                             (:file "io")
                             (:file "document")
                             (:file "disk"))))
  :in-order-to ((test-op (test-op "obj/tests"))))

(defsystem :obj/tests
  :depends-on (:rt :obj)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :obj)))
