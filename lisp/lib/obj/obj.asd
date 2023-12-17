(defsystem :obj
  :description "Lisp object library"
  :depends-on (:std)
  :components ((:file "pkg")
               (:file "id")
               (:file "seq")
               (:file "color")
               (:file "tbl"))
  :in-order-to ((test-op (test-op "obj/tests"))))

(defsystem :obj/tests
  :depends-on (:rt :obj)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :obj)))
