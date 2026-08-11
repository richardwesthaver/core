;;; rocksdb/metadata.lisp --- RocksDB Metadata

;;

;;; Code:
(in-package :rocksdb)

(defar rocksdb-get-column-family-metadata (* rocksdb-column-family-metadata)
  (db (* rocksdb)))

(defar rocksdb-get-column-family-metadata-cf (* rocksdb-column-family-metadata)
  (db (* rocksdb)) (cf (* rocksdb-column-family-handle)))

(defar rocksdb-column-family-metadata-destroy void
  (cf-meta (* rocksdb-column-family-metadata)))

(defar rocksdb-column-family-metadata-get-size (unsigned 64)
  (cf-meta (* rocksdb-column-family-metadata)))

(defar rocksdb-column-family-metadata-get-file-count size-t
  (cf-meta (* rocksdb-column-family-metadata)))

(defar rocksdb-column-family-metadata-get-name c-string
  (cf-meta (* rocksdb-column-family-metadata)))

(defar rocksdb-column-family-metadata-get-level-count size-t
  (cf-meta (* rocksdb-column-family-metadata)))

(defar rocksdb-column-family-metadata-get-level-metadata (* rocksdb-level-metadata)
  (cf-meta (* rocksdb-column-family-metadata)) (i size-t))

(defar rocksdb-level-metadata-destroy void (lmeta (* rocksdb-level-metadata)))

(defar rocksdb-level-metadata-get-level int (lmeta (* rocksdb-level-metadata)))

(defar rocksdb-level-metadata-get-size (unsigned 64) (lmeta (* rocksdb-level-metadata)))

(defar rocksdb-level-metadata-get-file-count size-t (lmeta (* rocksdb-level-metadata)))

(defar rocksdb-level-metadata-get-sst-file-metadata (* rocksdb-sst-file-metadata)
  (lmeta (* rocksdb-level-metadata)) 
  (i size-t))

(defar rocksdb-sst-file-metadata-destroy void
  (fmeta (* rocksdb-sst-file-metadata)))

(defar rocksdb-sst-file-metadata-get-relative-filename c-string
  (fmeta (* rocksdb-sst-file-metadata)))

(defar rocksdb-sst-file-metadata-get-directory c-string
  (fmeta (* rocksdb-sst-file-metadata)))

(defar rocksdb-sst-file-metadata-get-size (unsigned 64)
  (fmeta (* rocksdb-sst-file-metadata)))

(defar rocksdb-sst-file-metadata-get-smallestkey (array unsigned-char)
  (fmeta (* rocksdb-sst-file-metadata))
  (len (* size-t)))

(defar rocksdb-sst-file-metadata-get-largestkey (array unsigned-char)
  (fmeta (* rocksdb-sst-file-metadata))
  (len (* size-t)))

(defar rocksdb-export-import-files-metadata-create (* rocksdb-export-import-files-metadata))
(defar rocksdb-export-import-files-metadata-destroy void (obj (* rocksdb-export-import-files-metadata)))
(defar rocksdb-export-import-files-metadata-get-db-comparator-name c-string
  (opt (* rocksdb-export-import-files-metadata)))
