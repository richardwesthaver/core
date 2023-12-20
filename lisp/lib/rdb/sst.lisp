;;; lib/rdb/sst.lisp --- Sorted Sequence Tables

;; SST is the on-disk format for RocksDB data. Keys are (usually)
;; sorted. An SST-FILE can be generated independently of a database
;; and ingested on-demand.

;;; Code:
(in-package :rdb)

(defstruct sst-file ())

(defclass sst-stream (sb-gray:fundamental-binary-input-stream)
  ())
