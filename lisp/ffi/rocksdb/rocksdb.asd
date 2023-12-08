;;; rocksdb.asd --- ROCKSDB SYSTEMS

;; rocksdb for lisp.

;;; Commentary:

;; inspired by Vee's cl-rocksdb: https://github.com/veer66/cl-rocksdb

;;; Code:
(defsystem "rocksdb"
  :depends-on (:std)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "rocksdb/tests"))))

(defsystem "rocksdb/tests"
  :depends-on (:std/rt :rocksdb)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :rocksdb)))
