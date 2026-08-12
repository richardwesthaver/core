;;; rocksdb/sst.lisp --- SST Files

;; Functions for writing and ingesting SST Files directly.

;; ref: https://github.com/facebook/rocksdb/wiki/Creating-and-Ingesting-SST-files

;;; Code:
(in-package :rocksdb)

(defar rocksdb-sst-partitioner-fixed-prefix-factory-create (* rocksdb-sst-partitioner-factory)
  (prefix-len size-t))

(defar rocksdb-sstfilewriter-create (* rocksdb-sstfilewriter)
  (env-opts (* rocksdb-envoptions))
  (io-options (* rocksdb-options)))

(defar rocksdb-sstfilewriter-create-with-comparator (* rocksdb-sstfilewriter)
  (env-opts (* rocksdb-envoptions))
  (io-options (* rocksdb-options))
  (comparator (* rocksdb-comparator)))

(defar rocksdb-sstfilewriter-destroy void (writer (* rocksdb-sstfilewriter)))

(def-with-errptr rocksdb-sstfilewriter-open void
  (writer (* rocksdb-sstfilewriter))
  (name c-string))

(def-with-errptr rocksdb-sstfilewriter-add void
  (writer (* rocksdb-sstfilewriter))
  (key (* unsigned-char))
  (keylen size-t)
  (val (* unsigned-char))
  (vallen size-t))

(def-with-errptr rocksdb-sstfilewriter-put void
  (writer (* rocksdb-sstfilewriter))
  (key (* unsigned-char))
  (keylen size-t)
  (val (* unsigned-char))
  (vallen size-t))

(def-with-errptr rocksdb-sstfilewriter-put-with-ts void
  (writer (* rocksdb-sstfilewriter))
  (key (* char))
  (keylen size-t)
  (ts (* char))
  (tslen size-t)
  (val (* char))
  (vallen size-t))

(def-with-errptr rocksdb-sstfilewriter-merge void
  (writer (* rocksdb-sstfilewriter))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t))

(def-with-errptr rocksdb-sstfilewriter-delete void
  (writer (* rocksdb-sstfilewriter))
  (key (* char))
  (keylen size-t))

(def-with-errptr rocksdb-sstfilewriter-delete-with-ts void
  (writer (* rocksdb-sstfilewriter))
  (key (* char))
  (keylen size-t)
  (ts (* char))
  (tslen size-t))

(def-with-errptr rocksdb-sstfilewriter-delete-range void
  (writer (* rocksdb-sstfilewriter))
  (begin-key (* char))
  (begin-keylen size-t)
  (end-key (* char))
  (end-keylen size-t))

(def-with-errptr rocksdb-sstfilewriter-finish void
  (writer (* rocksdb-sstfilewriter)))

(def-with-errptr rocksdb-sstfilewriter-file-size void
  (writer (* rocksdb-sstfilewriter))
  (file-size (* unsigned-long)))

(def-with-errptr rocksdb-ingest-external-file void
  (db (* rocksdb))
  (file-list (* c-string))
  (list-len size-t)
  (opt (* rocksdb-ingestexternalfileoptions)))

(def-with-errptr rocksdb-ingest-external-file-cf void
  (db (* rocksdb))
  (cf-handle (* rocksdb-column-family-handle))
  (file-list (* c-string))
  (list-len size-t)
  (opt (* rocksdb-ingestexternalfileoptions)))

(def-with-errptr rocksdb-try-catch-up-with-primary void
  (db (* rocksdb)))

(defar rocksdb-sst-file-manager-create (* rocksdb-sst-file-manager) (env (* rocksdb-env)))
(defar rocksdb-sst-file-manager-destroy void (sfm (* rocksdb-sst-file-manager)))
(defar rocksdb-sst-file-manager-set-max-allowed-space-usage void
  (sfm (* rocksdb-sst-file-manager))
  (max-allowed-space unsigned-long))
(defar rocksdb-sst-file-manager-set-compaction-buffer-size void
  (sfm (* rocksdb-sst-file-manager))
  (compaction-buffer-size unsigned-long))
(defar rocksdb-sst-file-manager-is-max-allowed-space-reached boolean
  (sfm (* rocksdb-sst-file-manager)))
(defar rocksdb-sst-file-manager-is-max-allowed-space-reached-including-compactions boolean
  (sfm (* rocksdb-sst-file-manager)))
(defar rocksdb-sst-file-manager-get-total-size unsigned-long (sfm (* rocksdb-sst-file-manager)))
(defar rocksdb-sst-file-manager-get-delete-rate-bytes-per-second long
  (sfm (* rocksdb-sst-file-manager)))
(defar rocksdb-sst-file-manager-get-max-trash-db-ration double
  (sfm (* rocksdb-sst-file-manager)))
(defar rocksdb-sst-file-manager-set-max-trash-db-ration void
  (sfm (* rocksdb-sst-file-manager))
  (ratio double))
(defar rocksdb-sst-file-manager-get-total-trash-size unsigned-long
  (sfm (* rocksdb-sst-file-manager)))
