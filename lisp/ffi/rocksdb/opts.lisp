(in-package :rocksdb)

(define-opaque rocksdb-block-based-table-options)

(define-alien-routine rocksdb-block-based-options-set-block-cache void
  (opt (* rocksdb-block-based-table-options)) (block-cache (* rocksdb-cache)))

(define-alien-routine rocksdb-block-based-options-set-cache-index-and-filter-blocks void
  (opt (* rocksdb-block-based-table-options)) (cache-index-and-filter-blocks c-string))

(define-opt rocksdb-options
  create-if-missing
  create-missing-column-families
  error-if-exists
  paranoid-checks
  (info-log-level (val int))
  (write-buffer-size (val size-t))
  (db-write-buffer-size (val size-t))
  (max-open-files (val int))
  (max-file-opening-threads (val int))
  (max-total-wal-size (n unsigned-long))
  (compression-options (a int) (b int) (c int) (d int))
  (compression-options-zstd-max-train-bytes (val int))
  compression-options-use-zstd-dict-trainer
  compression-options-parallel-threads
  (compression-options-max-dict-buffer-bytes (val unsigned-long))
  (num-levels (val int))
  (level0-file-num-compaction-trigger (val int))
  (level0-slowdown-writes-trigger (val int))
  (level0-stop-writes-trigger (val int))
  (target-file-size-base (val unsigned-long))
  (target-file-size-multiplier (val int))
  (max-bytes-for-level-base (val unsigned-long))
  level-compaction-dynamic-level-bytes
  (max-bytes-for-level-multiplier (val double))
  (block-based-table-factory
   (table-options (* rocksdb-block-based-table-options)))
  (allow-ingest-behind (val unsigned-char))
  (merge-operator (comparator (* rocksdb-comparator)))
  (statistics-level (level int))
  (skip-stats-update-on-db-open (val unsigned-char))
  (skip-checking-sst-filie-sizes-on-db-open (val unsigned-char))
  (enable-blob-files (val unsigned-char))
  (min-blob-size (val unsigned-long))
  (blob-file-size (val unsigned-long))
  (blob-compression-type (val int))
  (enable-blob-gc (val unsigned-char))
  (blob-gc-age-cutoff (val double))
  (blob-gc-force-threshold (val double))
  (blob-compaction-readahead-size (val unsigned-long))
  (blob-file-starting-level (val int))
  (prepopulate-blob-cache (val int)))

(define-alien-routine rocksdb-options-enable-statistics void
  (* rocksdb-options))
;; (define-alien-routine rocksdb-options-set-db-paths void
;;   (opt (* rocksdb-options)))

(define-alien-routine rocksdb-options-set-blob-cache void
  (opt (* rocksdb-options)) (blob-cache (* rocksdb-cache)))

(define-opt rocksdb-writeoptions)
(define-opt rocksdb-readoptions)
(define-opt rocksdb-flushoptions
    (wait (val unsigned-char)))

(define-alien-routine rocksdb-options-increase-parallelism void 
      (opt (* rocksdb-options)) (total-threads int))

(define-alien-routine rocksdb-options-optimize-level-style-compaction void 
      (opt (* rocksdb-options))
      (memtable-memory-budget unsigned-long))

(define-alien-routine rocksdb-flushoptions-get-wait unsigned-char (* rocksdb-flushoptions))

(def-with-errptr rocksdb-load-latest-options 
  void
  (db-path c-string)
  (env (* rocksdb-env))
  (ignore-unknown-options boolean)
  (cache (* rocksdb-cache))
  (db-options (* (* rocksdb-options)))
  (num-column-families (* size-t))
  (column-family-names (* (* c-string)))
  (column-family-options (* (* (* rocksdb-options)))))

(define-alien-routine rocksdb-load-latest-options-destroy void
  (db-options (* (* rocksdb-options)))
  (list-column-family-names (* c-string))
  (list-column-family-options (* (* rocksdb-options)))
  (len size-t))

(def-with-errptr
  rocksdb-set-options 
  void
  (db (* rocksdb))
  (count int)
  (keys (array c-string))
  (values (array c-string)))

(def-with-errptr rocksdb-set-options-cf 
  void
  (db (* rocksdb))
  (handle (* rocksdb-column-family-handle))
  (count int)
  (keys (array c-string))
  (values (array c-string)))
