(defsystem :obj
  :description "Lisp object library"
  :depends-on (:std)
  :serial t
  :components ((:file "pkg")
               (:file "hash")
               (:file "id")
               (:file "seq")
               (:file "tree")
               (:file "graph")
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
