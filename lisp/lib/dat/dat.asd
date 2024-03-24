(defsystem :dat
  :description "Data formats"
  :depends-on (:cl-ppcre :std :obj :png)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg")
               (:file "proto")
               (:file "sxp")
               (:file "csv")
               (:file "json")
               (:module "xml"
                :components
                ((:file "xml")
                 (:file "pkg")
                 (:file "svg")
                 (:file "fixml")))
               (:file "toml")
               (:file "arff")
               (:file "midi")
               #+nil (:file "bencode"))
  :in-order-to ((test-op (test-op "dat/tests"))))

(defsystem :dat/tests
  :depends-on (:rt :dat)
  :perform (test-op (o c) (symbol-call :rt :do-tests :dat)))
