;;; q.asd --- Query Systems
(defsystem :q
  :description "Query System"
  :depends-on (:std :obj :log :dat :parse)
  :components ((:file "pkg")
               (:file "query")
               (:file "parser")
               (:file "engine")
               (:file "sql")
               (:file "dql")
               (:file "readtable"))
  :in-order-to ((test-op (test-op "q/tests"))))

(defsystem :q/tests
  :depends-on (:std :rt :q :log)
  :components ((:module "tests"
                :components ((:file "pkg")
                             (:file "fuzz")
                             (:file "suite"))))
  :in-order-to ((test-op (test-op "q/tests"))))
