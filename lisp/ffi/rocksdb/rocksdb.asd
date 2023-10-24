;;; rocksdb.asd --- ROCKSDB SYSTEMS

;; rocksdb for lisp.

;;; Commentary:

;; 

;;; Code:
(defsystem "rocksdb"
  :description "based on Vee's cl-rocksdb: https://github.com/veer66/cl-rocksdb/tree/main"
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on (:std :rocksdb/pkg)
  :in-order-to ((test-op (test-op "rocksdb/tests")))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :rocksdb)))
