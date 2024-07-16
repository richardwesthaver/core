(defsystem :dat
  :description "Data formats"
  :depends-on (:cl-ppcre :std :obj #+png :png :flexi-streams :io)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg")
               (:file "proto")
               (:file "sxp")
               (:file "dot")
               (:file "csv")
               (:file "json")
               (:module "parquet"
                :components
                ((:file "gen")
                 (:file "pkg")
                 (:file "obj")
                 (:file "tcompact")
                 (:file "io")
                 (:file "rle")
                 (:file "proto")))
               (:module "xml"
                :components
                ((:file "xml")
                 (:file "pkg")
                 (:file "svg")
                 (:file "fixml")))
               (:module "html"
                :components
                ((:file "const")
                 (:file "entity")
                 (:file "html")))
               (:file "mime")
               (:file "toml")
               (:file "arff")
               (:file "midi")
               #+png (:file "png")
               (:file "base64")
               #+bencode (:file "bencode"))
  :in-order-to ((test-op (test-op "dat/tests"))))

(defsystem :dat/tests
  :depends-on (:rt :dat)
  :perform (test-op (o c) (symbol-call :rt :do-tests :dat)))
