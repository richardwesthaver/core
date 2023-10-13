;;; rocksdb.asd --- ROCKSDB SYSTEMS

;; rocksdb for lisp.

;;; Commentary:

;; based on Vee's cl-rocksdb: https://github.com/veer66/cl-rocksdb/tree/main

;;; Code:
(defsystem "rocksdb"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on (:macs)
  :in-order-to ((test-op (test-op "rocksdb/tests")))
  :components ((:module "rocksdb"
                :serial t
                :components
                ((:file "rocksdb")
                 (:file "rdb")))))

(defsystem "rocksdb/tests"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/nas-t/issues"
  :depends-on (:rocksdb :rt)
  :components ((:file "rocksdb/tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :rocksdb)))
