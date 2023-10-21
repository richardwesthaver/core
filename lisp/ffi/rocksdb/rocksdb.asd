;;; rocksdb.asd --- ROCKSDB SYSTEMS

;; rocksdb for lisp.

;;; Commentary:

;; 

;;; Code:
(defsystem "rocksdb"
  :depends-on (:std :rocksdb/macs)
  :description "based on Vee's cl-rocksdb: https://github.com/veer66/cl-rocksdb/tree/main"
  :class :package-inferred-system
  :in-order-to ((test-op (test-op "rocksdb/tests")))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :rocksdb)))
