(in-package :rocksdb)
(defvar *rocksdb-compression-backends* '(snappy zlib bz2 lz4 lz4hc xpress zstd))

(defvar *rocksdb-compaction-levels* '(level universal fifo))

(defvar *rocksdb-perf-metrics*
  '(user-key-comparison-count block-cache-hit-count
    block-read-count block-read-byte
    block-read-time block-checksum-time
    block-decompress-time get-read-bytes
    multiget-read-bytes iter-read-bytes
    internal-key-skipped-count internal-delete-skipped-count
    internal-recent-skipped-count internal-merge-count
    get-snapshot-time get-from-memtable-time
    get-from-memtable-count get-post-process-time
    get-from-output-files-time seek-on-memtable-time
    seek-on-memtable-count next-on-memtable-count
    prev-on-memtable-count seek-child-seek-time
    seek-child-seek-count seek-min-heap-time
    seek-max-heap-time seek-internal-seek-time
    find-next-user-entry-time write-wal-time
    write-memtable-time write-delay-time
    write-pre-and-post-process-time db-mutex-lock-nanos
    db-condition-wait-nanos merge-operator-time-nanos
    read-index-block-nanos read-filter-block-nanos
    new-table-block-iter-nanos new-table-iterator-nanos
    block-seek-nanos find-table-nanos
    bloom-memtable-hit-count bloom-memtable-miss-count
    bloom-sst-hit-count bloom-sst-miss-count
    key-lock-wait-time key-lock-wait-count
    env-new-sequential-file-nanos env-new-random-access-file-nanos
    env-new-writable-file-nanos env-reuse-writable-file-nanos
    env-new-random-rw-file-nanos env-new-directory-nanos
    env-file-exists-nanos env-get-children-nanos
    env-get-children-file-attributes-nanos env-delete-file-nanos
    env-create-dir-nanos env-create-dir-if-missing-nanos
    env-delete-dir-nanos env-get-file-size-nanos
    env-get-file-modification-time-nanos env-rename-file-nanos
    env-link-file-nanos env-lock-file-nanos
    env-unlock-file-nanos env-new-logger-nanos
    number-async-seek blob-cache-hit-count
    blob-read-count blob-read-byte
    blob-read-time blob-checksum-time
    blob-decompress-time internal-range-del-reseek-count
    block-read-cpu-time total-metric-count))

(defvar *rocksdb-statistics-levels*
  (map 'vector
       (lambda (x) (string-downcase (symbol-name x)))
       '(disable-all except-tickers except-histogram-or-timers
         except-timers except-detailed-timers except-time-for-mutex
         all)))

(defvar *rocksdb-options*
  (map 'vector 
       (lambda (x) (string-downcase (symbol-name x)))
       '(create-if-missing create-missing-column-families error-if-exists
         paranoid-checks info-log-level write-buffer-size db-write-buffer-size
         max-open-files max-file-opening-threads max-total-wal-size compression-options
         compression-options-zstd-max-train-bytes compression-options-max-dict-buffer-bytes
         compression-options-parallel-threads compression-options-use-zstd-dict-trainer
         num-levels level0-file-num-compaction-trigger level0-slowdown-writes-trigger
         level0-stop-writes-trigger target-file-size-base target-file-size-multiplier 
         max-bytes-for-level-base level-compaction-dynamic-level-bytes max-bytes-for-level-multiplier
         block-based-table-factory allow-ingest-behind merge-operator statistics-level
         skip-stats-update-on-db-open skip-checking-sst-filie-sizes-on-db-open enable-blob-files
         min-blob-size blob-file-size blob-compression-type enable-blob-gc blob-gc-age-cutoff
         blob-gc-force-threshold blob-compaction-readahead-size blob-file-starting-level
         prepopulate-blob-cache))
  "Provides early list of options for macros to populate.")

(define-opt rocksdb-ingestexternalfileoptions)
(define-opt rocksdb-backup-engine-options)
(define-opt rocksdb-restore-options)
(define-opt rocksdb-hyper-clock-cache-options)
(define-opt rocksdb-fifo-compaction-options)
(define-opt rocksdb-transactiondb-options)
(define-opt rocksdb-transaction-options)
(define-opt rocksdb-optimistictransaction-options)
(define-opt rocksdb-envoptions)
(define-opt rocksdb-universal-compaction-options)

(define-opaque rocksdb-wal-readoptions)

(define-opaque rocksdb-block-based-table-options)
(define-alien-routine rocksdb-block-based-options-create (* rocksdb-block-based-table-options))
(define-alien-routine rocksdb-block-based-options-destroy void (self (* rocksdb-block-based-table-options)))

(define-alien-routine rocksdb-block-based-options-set-checksum void
  (opt (* rocksdb-block-based-table-options)) (val char))

(define-alien-routine rocksdb-block-based-options-set-block-size void
  (opt (* rocksdb-block-based-table-options)) (block-size size-t))

(define-alien-routine rocksdb-block-based-options-set-block-size-deviation void
  (opt (* rocksdb-block-based-table-options)) (block-size-deviation int))

(define-alien-routine rocksdb-block-based-options-set-block-restart-interval void
  (opt (* rocksdb-block-based-table-options)) (block-restart-interval int))

(define-alien-routine rocksdb-block-based-options-set-index-block-restart-interval void
  (opt (* rocksdb-block-based-table-options)) (index-block-restart-interval char))

(define-alien-routine rocksdb-block-based-options-set-metadata-block-size void
  (opt (* rocksdb-block-based-table-options)) (metadata-block-size unsigned-long))

(define-alien-routine rocksdb-block-based-options-set-partition-filters void
  (opt (* rocksdb-block-based-table-options)) (partition-filters unsigned-char))

(define-alien-routine rocksdb-block-based-options-set-partition-filters-for-memory void
  (opt (* rocksdb-block-based-table-options)) (optimize-filters-for-memory unsigned-char))

(define-alien-routine rocksdb-block-based-options-set-use-delta-encoding void
  (opt (* rocksdb-block-based-table-options)) (use-delta-encoding unsigned-char))

(define-alien-routine rocksdb-block-based-options-set-no-block-cache void
  (opt (* rocksdb-block-based-table-options)) (no-block-cache unsigned-char))

(define-alien-routine rocksdb-block-based-options-set-block-cache void
  (opt (* rocksdb-block-based-table-options)) (block-cache (* rocksdb-cache)))

(define-alien-routine rocksdb-block-based-options-set-format-version void
  (opt (* rocksdb-block-based-table-options)) (val int))

(define-alien-routine rocksdb-block-based-options-set-index-type void
  (opt (* rocksdb-block-based-table-options)) (val int))

(define-alien-routine rocksdb-block-based-options-set-data-block-index-type void
  (opt (* rocksdb-block-based-table-options)) (val int))

(define-alien-routine rocksdb-block-based-options-set-data-block-hash-ratio void
  (opt (* rocksdb-block-based-table-options)) (val double))

(define-alien-routine rocksdb-block-based-options-set-cache-index-and-filter-blocks void
  (opt (* rocksdb-block-based-table-options)) (val unsigned-char))

(define-alien-routine rocksdb-block-based-options-set-cache-index-and-filter-blocks-with-high-priority void
  (opt (* rocksdb-block-based-table-options)) (val unsigned-char))

(define-alien-routine rocksdb-block-based-options-set-pin-l0-filter-and-index-blocks-in-cache void
  (opt (* rocksdb-block-based-table-options)) (val unsigned-char))

(define-alien-routine rocksdb-block-based-options-set-pin-top-level-index-and-filter void
  (opt (* rocksdb-block-based-table-options)) (val unsigned-char))

(define-opaque rocksdb-cuckoo-table-options)

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
  (comparator (val (* rocksdb-comparator)))
  (merge-operator (val (* rocksdb-mergeoperator)))
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
  (blob-cache (val (* rocksdb-cache)))
  (prepopulate-blob-cache (val int)))

(define-alien-routine rocksdb-options-increase-parallelism void 
  (opt (* rocksdb-options)) (total-threads int))

(define-alien-routine rocksdb-options-optimize-level-style-compaction void 
  (opt (* rocksdb-options))
  (memtable-memory-budget unsigned-long))

(define-alien-routine rocksdb-options-enable-statistics void
  (* rocksdb-options))

;; (define-alien-routine rocksdb-options-set-db-paths void
;;   (opt (* rocksdb-options)))

;; (define-alien-routine rocksdb-options-set-uint64add-merge-operator void
;;   (opt (* rocksdb-options)))

(define-opt rocksdb-writeoptions)
(define-opt rocksdb-readoptions)
(define-opt rocksdb-flushoptions
    (wait (val unsigned-char)))

(define-opt rocksdb-compactoptions
    (exclusive-manual-compaction (val unsigned-char))
  (bottommost-level-compaction (val unsigned-char))
  (change-level (val unsigned-char))
  (target-level (val int)))

(define-opt rocksdb-lru-cache-options)

(define-alien-routine rocksdb-lru-cache-options-set-capacity void
  (self (* rocksdb-lru-cache-options))
  (val size-t))

(define-alien-routine rocksdb-lru-cache-options-set-num-shard-bits void
  (self (* rocksdb-lru-cache-options))
  (val int))

(define-alien-routine rocksdb-lru-cache-options-set-memory-allocator void
  (self (* rocksdb-lru-cache-options))
  (val (* rocksdb-memory-allocator)))

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

(define-alien-routine rocksdb-options-create-copy (* rocksdb-options)
  (src (* rocksdb-options)))
