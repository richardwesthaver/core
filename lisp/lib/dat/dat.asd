(defsystem :dat
  :description "Data Systems"
  :depends-on (:cl-ppcre :std :obj #+png :png :flexi-streams :io :log)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg")
               (:file "condition")
               (:file "sxp")
               (:file "dot")
               (:file "csv")
               (:file "json")
               (:file "id3")
               (:module "parquet"
                :components
                ((:file "gen")
                 (:file "pkg")
                 (:file "obj")
                 (:file "thrift")
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
               (:file "handlebars")
               (:file "mime")
               (:file "toml")
               (:file "arff")
               (:file "tar")
               (:file "midi")
               (:file "png")
               (:file "base64")
               (:file "dat"))
  :in-order-to ((test-op (test-op "dat/tests"))))

(defsystem :dat/tests
  :depends-on (:rt :dat)
  :perform (test-op (o c) (symbol-call :rt :do-tests :dat)))
