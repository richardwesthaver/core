;;; rocksdb.asd --- ROCKSDB SYSTEMS

;; rocksdb for lisp.

;;; Commentary:

;; 

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :rocksdb.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :rocksdb.sys)

(defsystem "rocksdb"
  :description "based on Vee's cl-rocksdb: https://github.com/veer66/cl-rocksdb/tree/main"
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :rocksdb))
  :in-order-to ((test-op (test-op "rocksdb/tests"))))

(defsystem "rocksdb/tests"
  :depends-on (:std/rt :rocksdb)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :rocksdb)))
