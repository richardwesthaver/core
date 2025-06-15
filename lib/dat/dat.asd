(defsystem :dat
  :description "Data Systems"
  :depends-on (:obj :io :log)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg")
               (:file "condition")
               (:file "asn1")
               (:file "sxp")
               (:file "dot")
               (:file "csv")
               (:file "json")
	       (:file "m3u")
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
                 (:file "svg")
                 (:file "fixml")))
               (:module "html"
                :components
                ((:file "const")
                 (:file "entity")
                 (:file "html")))
               (:file "yaml")
               (:file "mime")
               (:file "ini")
               (:file "toml")
               (:file "arff")
               (:file "tar")
               (:file "midi")
               (:file "png")
               (:file "base64")
               (:file "qrcode"))
  :in-order-to ((test-op (test-op "dat/tests"))))

(defsystem :dat/tests
  :depends-on (:rt :dat)
  :perform (test-op (o c) (symbol-call :rt :do-tests :dat)))
