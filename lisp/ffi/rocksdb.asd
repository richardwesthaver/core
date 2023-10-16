;;; rocksdb.asd --- ROCKSDB SYSTEMS

;; rocksdb for lisp.

;;; Commentary:

;; based on Vee's cl-rocksdb: https://github.com/veer66/cl-rocksdb/tree/main

;;; Code:
(defsystem "rocksdb"
  :depends-on (:macs)
  :in-order-to ((test-op (test-op "rocksdb/tests")))
  :components ((:file "rocksdb/rocksdb")))

(defsystem "rocksdb/tests"
  :depends-on (:rocksdb :rt)
  :components ((:file "rocksdb/tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :rocksdb)))
