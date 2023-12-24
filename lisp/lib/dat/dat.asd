(defsystem :dat
  :description "Data formats"
  :depends-on (:std :obj)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg")
               (:file "csv")
               (:file "json")
               (:file "xml")
               (:file "toml")
               (:file "arff")
               #+nil (:file "bencode"))
  :in-order-to ((test-op (test-op "dat/tests"))))

(defsystem :dat/tests
  :depends-on (:rt :dat)
  :perform (test-op (o c) (symbol-call :rt :do-tests :dat)))
