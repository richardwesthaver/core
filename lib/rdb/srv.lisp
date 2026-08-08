;;; srv.lisp --- Services

;; Service Objects backed by a RocksDB instance.

;;; Code:
(in-package :rdb)

(defservice rdb-service (service rdb)
  ()
  (:default-initargs :db :rdb))

;;; Remote Compaction
