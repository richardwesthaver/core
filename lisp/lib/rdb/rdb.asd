;;; rdb.asd --- thin RocksDB ORM
(defsystem "rdb"
  :version "0.1.0"
  :license (:file "LICENSE")
  :class :package-inferred-system
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/core/issues"
  :depends-on (:std :rocksdb)
  :in-order-to ((test-op (test-op "rdb/tests")))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :rdb))
  :components ((:file "rdb")))
