;;; rocksdb.asd --- ROCKSDB SYSTEMS

;; RocksDB for lisp.

;; inspired by Vee's cl-rocksdb: https://github.com/veer66/cl-rocksdb

;;; Code:
(defsystem "rocksdb"
  :depends-on (:std :log)
  :serial t
  :components ((:file "pkg")
               (:file "prim")
               (:file "types")
               (:file "opts")
               (:file "env")
               (:file "sst")
               (:file "db")
               (:file "metadata")
               (:file "merge")
               (:file "compaction")
               (:file "comparator")
               (:file "slicetransform")
               (:file "writebatch")
               (:file "logger")
               (:file "stats")
               (:file "vars")
               (:file "macs"))
  :in-order-to ((test-op (test-op "rocksdb/tests"))))

(defsystem "rocksdb/tests"
  :depends-on (:rt :rocksdb)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :rocksdb)))
