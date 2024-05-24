(defsystem :obj
  :description "Lisp object library"
  :depends-on (:std :cli)
  :serial t
  :components ((:file "pkg")
               (:module "meta"
                :components ((:file "pkg")
                             (:file "sealed")
                             (:file "stealth")
                             (:file "typed")
                             (:file "filtered")
                             (:file "fast")
                             (:file "lazy")
                             (:file "overloaded")))
               (:module "hash"
                :components ((:file "hasher")
                             (:file "map")
                             (:file "set")
                             (:file "chash")
                             (:file "castable")))
               (:file "id")
               (:module "uri"
                :components ((:file "pkg")
                             (:file "domain")
                             (:file "uri")
                             (:file "mask")
                             (:file "state")
                             (:file "parse")
                             (:file "print")
                             (:file "path")
                             (:file "intern")))
               (:file "url")
               (:file "seq")
               (:module "tree"
                :components ((:file "node")
                             (:file "avl")
                             (:file "rb")
                             (:file "bro")))
               (:module "graph"
                :components ((:file "pkg")))
               (:file "equiv")
               (:module "color"
                :components ((:file "color")
                             (:file "palette")
                             (:file "util")
                             (:file "x11-colors")))
               (:module "music"
                :components ((:file "music")))
               (:module "time"
                :components ((:file "local")))
               (:file "uuid")
               (:file "temperature")
               (:file "direction")
               (:file "shape")
               (:file "tbl")
               (:module "db"
                :components ((:file "mop")
                             (:file "proto")
                             (:file "io")
                             (:file "document")
                             (:file "disk")))
               (:file "cfg")
               (:file "build"))
  :in-order-to ((test-op (test-op "obj/tests"))))

(defsystem :obj/tests
  :depends-on (:rt :obj)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :obj)))
