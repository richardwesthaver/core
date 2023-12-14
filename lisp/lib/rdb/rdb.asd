;;; rdb.asd --- thin RocksDB ORM
(defsystem "rdb"
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on (:std :rocksdb)
  :serial t
  :components ((:file "pkg")
               (:file "err") 
               (:file "proto")
               (:file "util")
               (:file "obj")
               (:file "macs"))
  :in-order-to ((test-op (test-op :rdb/tests))))

(defsystem :rdb/tests
  :depends-on (:rt :rdb :rocksdb)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :rdb)))

