;;; q.asd --- Query Systems
(defsystem :q
  :description "Query System"
  :depends-on (:std :obj :log :dat :parse)
  :components ((:file "pkg")
               (:file "sql" :depends-on "pkg")
               (:file "lql" :depends-on "pkg"))
  :in-order-to ((test-op (test-op "q/tests"))))

(defsystem :q/tests
  :depends-on (:std :rt :q :log)
  :components ((:file "tests"))
  :in-order-to ((test-op (test-op "q/tests"))))
