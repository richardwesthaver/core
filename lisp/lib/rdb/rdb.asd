;;; rdb.asd --- thin RocksDB ORM
(defsystem "rdb"
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/comp/core/issues"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:std :rocksdb :rdb/pkg)
  :in-order-to ((test-op (test-op :rdb/tests)))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :rdb)))

