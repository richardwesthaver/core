;;; rdb.asd --- thin RocksDB ORM
(defsystem "rdb"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/core/issues"
  :depends-on (:macs :rocksdb)
  :in-order-to ((test-op (test-op "rdb/tests")))
  :components ((:file "rdb/rdb")))

(defsystem "rdb/tests"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/core/issues"
  :depends-on (:rdb :rt)
  :components ((:file "rdb/tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :rdb)))
