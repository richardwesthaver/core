(defsystem :xdb
  :depends-on (:std :obj)
  :serial t
  :components ((:file "xdb"))
  :in-order-to ((test-op (test-op "xdb/tests"))))

(defsystem :xdb/tests
  :depends-on (:rt :obj :xdb)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :xdb)))
