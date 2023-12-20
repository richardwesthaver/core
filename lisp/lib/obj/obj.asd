(defsystem :obj
  :description "Lisp object library"
  :depends-on (:std)
  :serial t
  :components ((:file "pkg")
               (:file "uri")
               (:file "hash")
               (:file "id")
               (:file "seq")
               (:file "tree")
               (:file "graph")
               (:file "color")
               (:file "tbl")
               (:file "db"))
  :in-order-to ((test-op (test-op "obj/tests"))))

(defsystem :obj/tests
  :depends-on (:rt :obj)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :obj)))
