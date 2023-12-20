(in-package :rocksdb)

(define-opt rocksdb-ingestexternalfileoptions)
(define-alien-routine rocksdb-ingestexternalfileoptions-set-move-files void
  (val unsigned-char))
(define-alien-routine rocksdb-ingestexternalfileoptions-set-snapshot-consistency void
  (val unsigned-char))
(define-alien-routine rocksdb-ingestexternalfileoptions-set-allow-global-seqno void
  (val unsigned-char))
(define-alien-routine rocksdb-ingestexternalfileoptions-set-allow-blocking-flush void
  (val unsigned-char))
(define-alien-routine rocksdb-ingestexternalfileoptions-set-ingest-behind void
  (val unsigned-char))
(define-alien-routine rocksdb-ingestexternalfileoptions-set-fail-if-not-bottommost-level void
  (val unsigned-char))
(export '(rocksdb-ingestexternalfileoptions-set-move-files 
          rocksdb-ingestexternalfileoptions-set-snapshot-consistency
          rocksdb-ingestexternalfileoptions-set-allow-global-seqno 
          rocksdb-ingestexternalfileoptions-set-allow-blocking-flush
          rocksdb-ingestexternalfileoptions-set-ingest-behind
          rocksdb-ingestexternalfileoptions-set-fail-if-not-bottommost-level))

(define-opt rocksdb-backup-engine-options)
(define-opt rocksdb-restore-options)
(define-opt rocksdb-hyper-clock-cache-options)
(define-opt rocksdb-fifo-compaction-options)
(define-opt rocksdb-transactiondb-options)
(define-opt rocksdb-transaction-options)
(define-opt rocksdb-optimistictransaction-options)
(define-opt rocksdb-envoptions)
(define-opt rocksdb-universal-compaction-options)

;;; WAL Read Options
(define-opaque rocksdb-wal-readoptions)
(export '(rocksdb-wal-readoptions))

;;; Block based Table Options
(eval-always
  (define-opaque rocksdb-block-based-table-options))

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

(export '(rocksdb-block-based-options-create rocksdb-block-based-options-destroy))
(export-opt-accessors rocksdb-block-based-options
                      checksum
                      block-size
                      block-size-deviation
                      block-restart-interval
                      index-block-restart-interval
                      metadata-block-size
                      partition-filters
                      partition-filters-for-memory
                      use-delta-encoding
                      no-block-cache
                      block-cache
                      format-version
                      index-type
                      data-block-index-type
                      data-block-hash-ratio
                      cache-index-and-filter-blocks
                      cache-index-and-filter-blocks-with-high-priority
                      pin-l0-filter-and-index-blocks-in-cache
                      pin-top-level-index-and-filter)

;;; Cuckoo Table Options
(define-opaque rocksdb-cuckoo-table-options)

;;; RocksDB Options
(define-opt rocksdb-options)
(define-opt-accessor rocksdb-options create-if-missing)
(define-opt-accessor rocksdb-options create-missing-column-families)
(define-opt-accessor rocksdb-options error-if-exists)
(define-opt-accessor rocksdb-options paranoid-checks)
(define-opt-accessor rocksdb-options compression-options-use-zstd-dict-trainer)
(define-opt-accessor rocksdb-options level-compaction-dynamic-level-bytes)
(define-opt-accessor rocksdb-options enable-blob-gc)
(define-opt-accessor rocksdb-options allow-ingest-behind)
(define-opt-accessor rocksdb-options skip-stats-update-on-db-open)
(define-opt-accessor rocksdb-options skip-checking-sst-file-sizes-on-db-open)
(define-opt-accessor rocksdb-options enable-blob-files)
(define-opt-accessor rocksdb-options enable-pipelined-write)
(define-opt-accessor rocksdb-options unordered-write)
(define-opt-accessor rocksdb-options allow-mmap-reads)
(define-opt-accessor rocksdb-options allow-mmap-writes)
(define-opt-accessor rocksdb-options use-direct-reads)
(define-opt-accessor rocksdb-options use-direct-io-for-flush-and-compaction)
(define-opt-accessor rocksdb-options is-fd-close-on-exec)
(define-opt-accessor rocksdb-options inplace-update-support)
(define-opt-accessor rocksdb-options advise-random-on-open)
(define-opt-accessor rocksdb-options atomic-flush)
(define-opt-accessor rocksdb-options manual-wal-flush)
(define-opt-accessor rocksdb-options avoid-unnecessary-blocking-io)

(define-opt-accessor rocksdb-options info-log-level int)
(define-opt-accessor rocksdb-options write-buffer-size size-t)
(define-opt-accessor rocksdb-options db-write-buffer-size size-t)
(define-opt-accessor rocksdb-options max-open-files int)
(define-opt-accessor rocksdb-options max-file-opening-threads int)
(define-opt-accessor rocksdb-options max-total-wal-size unsigned-long)
;; (define-opt-accessor rocksdb-options compression-options (a int)(b int) (c int) (d int))
(define-opt-accessor rocksdb-options compression-options-zstd-max-train-bytes int)
(define-opt-accessor rocksdb-options compression-options-max-dict-buffer-bytes unsigned-long)
(define-opt-accessor rocksdb-options num-levels int)
(define-opt-accessor rocksdb-options level0-file-num-compaction-trigger int)
(define-opt-accessor rocksdb-options level0-slowdown-writes-trigger int)
(define-opt-accessor rocksdb-options level0-stop-writes-trigger int)
(define-opt-accessor rocksdb-options target-file-size-base unsigned-long)
(define-opt-accessor rocksdb-options target-file-size-multiplier int)
(define-opt-accessor rocksdb-options max-bytes-for-level-base unsigned-long)
(define-opt-accessor rocksdb-options max-bytes-for-level-multiplier double)
(define-opt-accessor rocksdb-options block-based-table-factory (* rocksdb-block-based-table-options))
(define-opt-accessor rocksdb-options comparator (* rocksdb-comparator))
(define-opt-accessor rocksdb-options merge-operator (* rocksdb-mergeoperator))
(define-opt-accessor rocksdb-options statistics-level int)
(define-opt-accessor rocksdb-options min-blob-size unsigned-long)
(define-opt-accessor rocksdb-options blob-file-size unsigned-long)
(define-opt-accessor rocksdb-options blob-compression-type int)

(define-opt-accessor rocksdb-options blob-gc-age-cutoff double)
(define-opt-accessor rocksdb-options blob-gc-force-threshold double)
(define-opt-accessor rocksdb-options blob-compaction-readahead-size unsigned-long)
(define-opt-accessor rocksdb-options blob-file-starting-level int)
(define-opt-accessor rocksdb-options blob-cache (* rocksdb-cache))
(define-opt-accessor rocksdb-options prepopulate-blob-cache int)
(define-opt-accessor rocksdb-options max-write-buffer-number int)
(define-opt-accessor rocksdb-options min-write-buffer-number-to-merge int)
(define-opt-accessor rocksdb-options max-write-buffer-number-to-maintain int)
(define-opt-accessor rocksdb-options max-write-buffer-size-to-maintain long)
(define-opt-accessor rocksdb-options max-subcompactions unsigned-int)
(define-opt-accessor rocksdb-options max-background-jobs int)
(define-opt-accessor rocksdb-options max-background-compactions int)
(define-opt-accessor rocksdb-options max-background-flushes int)
(define-opt-accessor rocksdb-options max-log-file-size size-t)
(define-opt-accessor rocksdb-options log-file-time-to-roll size-t)
(define-opt-accessor rocksdb-options keep-log-file-num size-t)
(define-opt-accessor rocksdb-options recycle-log-file-num size-t)
(define-opt-accessor rocksdb-options soft-pending-compaction-bytes-limit size-t)
(define-opt-accessor rocksdb-options hard-pending-compaction-bytes-limit size-t)
(define-opt-accessor rocksdb-options max-manifest-file-size size-t)
(define-opt-accessor rocksdb-options table-cache-numshardbits int)
(define-opt-accessor rocksdb-options arena-block-size size-t)
(define-opt-accessor rocksdb-options use-fsync int)
(define-opt-accessor rocksdb-options db-log-dir c-string)
(define-opt-accessor rocksdb-options wal-dir c-string)
(define-opt-accessor rocksdb-options wal-ttl-seconds unsigned-long)
(define-opt-accessor rocksdb-options wal-size-limit-mb unsigned-long)
(define-opt-accessor rocksdb-options manifest-preallocation-size size-t)
(define-opt-accessor rocksdb-options stats-dump-period-sec unsigned-int)
(define-opt-accessor rocksdb-options stats-persist-period-sec unsigned-int)

(define-opt-accessor rocksdb-options access-hint-on-compaction-start int)
(define-opt-accessor rocksdb-options use-adaptive-mutex unsigned-char)
(define-opt-accessor rocksdb-options bytes-per-sync unsigned-long)
(define-opt-accessor rocksdb-options wal-bytes-per-sync unsigned-long)
(define-opt-accessor rocksdb-options file-max-buffer-size unsigned-long)
(define-opt-accessor rocksdb-options allow-concurrent-memtable-write)
(define-opt-accessor rocksdb-options enable-write-thread-adaptive-yield)
(define-opt-accessor rocksdb-options max-sequential-skip-in-iterations unsigned-long)
(define-opt-accessor rocksdb-options disable-auto-compaction int)
(define-opt-accessor rocksdb-options optimize-filters-for-hits int)
(define-opt-accessor rocksdb-options delete-obsolete-files-period-micros unsigned-long)
(define-opt-accessor rocksdb-options memtable-prefix-bloom-size-ration double)
(define-opt-accessor rocksdb-options max-compaction-bytes unsigned-long)
(define-opt-accessor rocksdb-options memtable-huge-page-size size-t)
(define-opt-accessor rocksdb-options max-successive-merges size-t)
(define-opt-accessor rocksdb-options bloom-locality unsigned-int)
(define-opt-accessor rocksdb-options report-bg-io-stats int)
(define-opt-accessor rocksdb-options experimental-mempurge-threshold double)
(define-opt-accessor rocksdb-options wal-recovery-mode int)
(define-opt-accessor rocksdb-options compression-options-parallel-threads int)
(define-opt-accessor rocksdb-options compression int)
(define-opt-accessor rocksdb-options bottommost-compression int)
(define-opt-accessor rocksdb-options compaction-style int)
(define-opt-accessor rocksdb-options wal-compression int)

;; (universal-compaction-options)
;; (ratelimiter)
;; (row-cache)
;; (hash-link-list-rep)
;; (plain-table-factory
;; (hash-skip-list-rep)
;; (prepare-for-bulk-load)
;; (memtable-vector-rep)

(define-alien-routine rocksdb-options-increase-parallelism void 
  (opt (* rocksdb-options)) (total-threads int))

(define-alien-routine rocksdb-options-optimize-level-style-compaction void 
  (opt (* rocksdb-options))
  (memtable-memory-budget unsigned-long))

(define-alien-routine rocksdb-options-enable-statistics void
  (* rocksdb-options))

(define-alien-routine rocksdb-options-statistics-get-string c-string
  (opt (* rocksdb-options)))

(define-alien-routine rocksdb-options-statistics-get-ticker-count unsigned-long
  (opt (* rocksdb-options))
  (ticker-type unsigned-int))

(define-alien-routine rocksdb-options-statistics-get-histogram-data void
  (opt (* rocksdb-options))
  (histogram-type unsigned-int)
  (data (* rocksdb-statistics-histogram-data)))

;; (define-alien-routine rocksdb-options-set-db-paths void
;;   (opt (* rocksdb-options)))

;; (define-alien-routine rocksdb-options-set-uint64add-merge-operator void
;;   (opt (* rocksdb-options)))

(export '(rocksdb-options-increase-parallelism rocksdb-options-optimize-level-style-compaction
          rocksdb-options-enable-statistics rocksdb-options-statistics-get-string
          rocksdb-options-statistics-get-ticker-count rocksdb-options-statistics-get-histogram-data))

;;; RocksDB Write Options
(define-opt rocksdb-writeoptions)
;;; RocksDB Read Options
(define-opt rocksdb-readoptions)
;;; RocksDB Flush Options
(define-opt rocksdb-flushoptions)
(define-opt-accessor rocksdb-flushoptions wait)
;;; RocksDB Compact Options
(define-opt rocksdb-compactoptions)
(define-opt-accessor rocksdb-compactoptions exclusive-manual-compaction)
(define-opt-accessor rocksdb-compactoptions bottommost-level-compaction)
(define-opt-accessor rocksdb-compactoptions change-level)
(define-opt-accessor rocksdb-compactoptions target-level int)
;;; RocksDB LRU Cache Options
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

(export-opt-accessors rocksdb-lru-cache-options
                      capacity
                      num-shard-bits
                      memory-allocator)

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
(export '(rocksdb-load-latest-options-destroy))

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
(export '(rocksdb-options-create-copy))
