;;; rocksdb/opts.lisp --- Rocksdb Options FFI

;; 

;;; Code:
(in-package :rocksdb)

(define-opt rocksdb-ingestexternalfileoptions)
(defar rocksdb-ingestexternalfileoptions-set-move-files void
  (opts (* rocksdb-ingestexternalfileoptions))
  (val boolean))
(defar rocksdb-ingestexternalfileoptions-set-snapshot-consistency void
  (opts (* rocksdb-ingestexternalfileoptions))
  (val boolean))
(defar rocksdb-ingestexternalfileoptions-set-allow-global-seqno void
  (opts (* rocksdb-ingestexternalfileoptions))
  (val boolean))
(defar rocksdb-ingestexternalfileoptions-set-allow-blocking-flush void
  (opts (* rocksdb-ingestexternalfileoptions))
  (val boolean))
(defar rocksdb-ingestexternalfileoptions-set-ingest-behind void
  (opts (* rocksdb-ingestexternalfileoptions))
  (val boolean))
(defar rocksdb-ingestexternalfileoptions-set-fail-if-not-bottommost-level void
  (opts (* rocksdb-ingestexternalfileoptions))
  (val boolean))

(export '(rocksdb-ingestexternalfileoptions-set-move-files 
          rocksdb-ingestexternalfileoptions-set-snapshot-consistency
          rocksdb-ingestexternalfileoptions-set-allow-global-seqno 
          rocksdb-ingestexternalfileoptions-set-allow-blocking-flush
          rocksdb-ingestexternalfileoptions-set-ingest-behind
          rocksdb-ingestexternalfileoptions-set-fail-if-not-bottommost-level))

(define-opt rocksdb-backup-engine-options)
(defar rocksdb-backup-engine-options-set-backup-dir void
  (opts (* rocksdb-backup-engine-options)) (backup-dir c-string))
(defar rocksdb-backup-engine-options-set-env void
  (opts (* rocksdb-backup-engine-options)) (val boolean))
(define-opt-accessor rocksdb-backup-engine-options share-table-files)
(define-opt-accessor rocksdb-backup-engine-options sync)
(define-opt-accessor rocksdb-backup-engine-options destroy-old-data)
(define-opt-accessor rocksdb-backup-engine-options backup-log-files)
(define-opt-accessor rocksdb-backup-engine-options backup-rate-limit (unsigned 64))
(define-opt-accessor rocksdb-backup-engine-options restore-rate-limit (unsigned 64))
(define-opt-accessor rocksdb-backup-engine-options callback-trigger-interval-size (unsigned 64))
(define-opt-accessor rocksdb-backup-engine-options max-valid-backups-to-open int)
(define-opt-accessor rocksdb-backup-engine-options shared-files-with-checksum-naming int)
(define-opt rocksdb-restore-options)
(defar rocksdb-restore-options-set-keep-log-files void
  (opts (* rocksdb-restore-options))
  (v int))

(define-opt rocksdb-hyper-clock-cache-options)
(defar rocksdb-hyper-clock-cache-options-set-capacity void
  (opts (* rocksdb-hyper-clock-cache-options))
  (v size-t))
(defar rocksdb-hyper-clock-cache-options-set-estimated-entry-charge void
  (opts (* rocksdb-hyper-clock-cache-options))
  (v size-t))
(defar rocksdb-hyper-clock-cache-options-set-num-shard-bits void
  (opts (* rocksdb-hyper-clock-cache-options))
  (v int))
(defar rocksdb-hyper-clock-cache-options-set-memory-allocator void
  (opts (* rocksdb-hyper-clock-cache-options))
  (malloc (* rocksdb-memory-allocator)))

(define-alien-enum (rocksdb-txndb-write-policy)
  :write-commited 0
  :write-prepared 1
  :write-unprepared 2)

(define-opt rocksdb-transactiondb-options)
(defar rocksdb-transactiondb-options-set-write-policy void
  (opt (* rocksdb-transactiondb-options))
  (write-policy rocksdb-txndb-write-policy))

(define-opt rocksdb-transaction-options)
(define-opt rocksdb-optimistictransaction-options)
(define-opt rocksdb-envoptions)
(define-opt rocksdb-universal-compaction-options)
(defar rocksdb-universal-compaction-options-set-size-ratio void
  (opt (* rocksdb-universal-compaction-options))
  (val int))
(defar rocksdb-universal-compaction-options-get-size-ratio int
  (opt (* rocksdb-universal-compaction-options)))

(defar rocksdb-universal-compaction-options-set-min-merge-width void
  (opt (* rocksdb-universal-compaction-options))
  (val int))
(defar rocksdb-universal-compaction-options-get-min-merge-width int
  (opt (* rocksdb-universal-compaction-options)))

(defar rocksdb-universal-compaction-options-set-max-merge-width void
  (opt (* rocksdb-universal-compaction-options))
  (val int))
(defar rocksdb-universal-compaction-options-get-max-merge-width int
  (opt (* rocksdb-universal-compaction-options)))

(defar rocksdb-universal-compaction-options-set-max-size-amplification-percent void
  (opt (* rocksdb-universal-compaction-options))
  (val int))
(defar rocksdb-universal-compaction-options-get-max-size-amplification-percent int
  (opt (* rocksdb-universal-compaction-options)))

(defar rocksdb-universal-compaction-options-set-compression-size-percent void
  (opt (* rocksdb-universal-compaction-options))
  (val int))
(defar rocksdb-universal-compaction-options-get-compression-size-percent-percent int
  (opt (* rocksdb-universal-compaction-options)))

(defar rocksdb-universal-compaction-options-set-stop-style void
  (opt (* rocksdb-universal-compaction-options))
  (val int))
(defar rocksdb-universal-compaction-options-get-stop-style int
  (opt (* rocksdb-universal-compaction-options)))

;;; WAL Read Options
(define-opaque rocksdb-wal-readoptions)

;;; Block based Table Options
(define-opaque rocksdb-block-based-table-options)

(defar rocksdb-block-based-options-create (* rocksdb-block-based-table-options))
(defar rocksdb-block-based-options-destroy void (self (* rocksdb-block-based-table-options)))
(defar rocksdb-block-based-options-set-checksum void
  (opt (* rocksdb-block-based-table-options)) (val char))
(defar rocksdb-block-based-options-set-block-size void
  (opt (* rocksdb-block-based-table-options)) (block-size size-t))
(defar rocksdb-block-based-options-set-block-size-deviation void
  (opt (* rocksdb-block-based-table-options)) (block-size-deviation int))
(defar rocksdb-block-based-options-set-block-restart-interval void
  (opt (* rocksdb-block-based-table-options)) (block-restart-interval int))
(defar rocksdb-block-based-options-set-index-block-restart-interval void
  (opt (* rocksdb-block-based-table-options)) (index-block-restart-interval char))
(defar rocksdb-block-based-options-set-metadata-block-size void
  (opt (* rocksdb-block-based-table-options)) (metadata-block-size unsigned-long))
(defar rocksdb-block-based-options-set-partition-filters void
  (opt (* rocksdb-block-based-table-options)) (partition-filters unsigned-char))
(defar rocksdb-block-based-options-set-partition-filters-for-memory void
  (opt (* rocksdb-block-based-table-options)) (optimize-filters-for-memory unsigned-char))
(defar rocksdb-block-based-options-set-use-delta-encoding void
  (opt (* rocksdb-block-based-table-options)) (use-delta-encoding unsigned-char))
(defar rocksdb-block-based-options-set-no-block-cache void
  (opt (* rocksdb-block-based-table-options)) (no-block-cache unsigned-char))
(defar rocksdb-block-based-options-set-block-cache void
  (opt (* rocksdb-block-based-table-options)) (block-cache (* rocksdb-cache)))
(defar rocksdb-block-based-options-set-format-version void
  (opt (* rocksdb-block-based-table-options)) (val int))
(define-alien-enum (rocksdb-block-based-table-index-type :type unsigned-char)
  :binary-search 0
  :hash-search 1
  :two-level-index-search 2)
(defar rocksdb-block-based-options-set-separate-key-value-in-data-block void
  (opt (* rocksdb-block-based-table-options)) (val rocksdb-block-based-table-index-type))
(defar rocksdb-block-based-options-set-index-type void
  (opt (* rocksdb-block-based-table-options)) (val int))
(defar rocksdb-block-based-options-set-data-block-index-type void
  (opt (* rocksdb-block-based-table-options)) (val int))
(defar rocksdb-block-based-options-set-data-block-hash-ratio void
  (opt (* rocksdb-block-based-table-options)) (val double))
(defar rocksdb-block-based-options-set-cache-index-and-filter-blocks void
  (opt (* rocksdb-block-based-table-options)) (val unsigned-char))
(defar rocksdb-block-based-options-set-cache-index-and-filter-blocks-with-high-priority void
  (opt (* rocksdb-block-based-table-options)) (val unsigned-char))
(defar rocksdb-block-based-options-set-pin-l0-filter-and-index-blocks-in-cache void
  (opt (* rocksdb-block-based-table-options)) (val unsigned-char))
(defar rocksdb-block-based-options-set-pin-top-level-index-and-filter void
  (opt (* rocksdb-block-based-table-options)) (val unsigned-char))
(defar rocksdb-block-based-options-set-top-level-index-pinning-tier void
  (opt (* rocksdb-block-based-table-options))
  (i int))
(defar rocksdb-block-based-options-set-partition-pinning-tier void
  (opt (* rocksdb-block-based-table-options))
  (i int))
(defar rocksdb-block-based-options-set-unpartition-pinning-tier void
  (opt (* rocksdb-block-based-table-options))
  (i int))

(defar rocksdb-block-based-options-set-uniform-cv-threshold void
  (opt (* rocksdb-block-based-table-options)) (val double))

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
(define-opt-accessor rocksdb-options level-compaction-dynamic-level-bytes (unsigned 8))
(define-opt-accessor rocksdb-options enable-blob-gc)
(define-opt-accessor rocksdb-options allow-ingest-behind)
(define-opt-accessor rocksdb-options skip-stats-update-on-db-open)
(define-opt-accessor rocksdb-options enable-blob-files)
(define-opt-accessor rocksdb-options enable-pipelined-write)
(define-opt-accessor rocksdb-options unordered-write)
(define-opt-accessor rocksdb-options allow-mmap-reads)
(define-opt-accessor rocksdb-options allow-mmap-writes)
(define-opt-accessor rocksdb-options use-direct-reads)
(define-opt-accessor rocksdb-options use-direct-io-for-flush-and-compaction)
(define-opt-accessor rocksdb-options is-fd-close-on-exec)
(define-opt-accessor rocksdb-options inplace-update-num-locks size-t)
(define-opt-accessor rocksdb-options inplace-update-support)
(define-opt-accessor rocksdb-options advise-random-on-open)
(define-opt-accessor rocksdb-options atomic-flush)
(define-opt-accessor rocksdb-options manual-wal-flush)
(define-opt-accessor rocksdb-options async-wal-precreate)
(define-opt-accessor rocksdb-options avoid-unnecessary-blocking-io)
(define-opt-accessor rocksdb-options writable-file-max-buffer-size (unsigned 64))
(define-opt-accessor rocksdb-options info-log-level int)
(define-opt-accessor rocksdb-options write-buffer-size size-t)
(define-opt-accessor rocksdb-options db-write-buffer-size size-t)
(define-opt-accessor rocksdb-options max-open-files int)
(define-opt-accessor rocksdb-options max-file-opening-threads int)
(define-opt-accessor rocksdb-options max-total-wal-size unsigned-long)
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
(define-opt-accessor rocksdb-options memtable-op-scan-flush-trigger (unsigned 32))
;; NOTE 2026-04-04: this option is incompatible with fifo-compaction
(define-opt-accessor rocksdb-options open-files-async unsigned-char)
(define-opt-accessor rocksdb-options read-triggered-compaction-threshold double)
(define-opt-accessor rocksdb-options max-compaction-trigger-wakeup-seconds unsigned-long)
(define-opt-accessor rocksdb-options min-tombstones-for-range-conversion unsigned-int)
(define-opt-accessor rocksdb-options memtable-batch-lookup-optimization)
(define-alien-enum (rocksdb-compression-type)
  :none 0
  :snappy 1
  :zlib 2
  :bz2 3
  :lz4 4
  :lz4hc 5
  :xpress 6
  :zstd 7)
(defar rocksdb-options-set-compression-options void
  (opt (* rocksdb-options))
  (a int) (b int) (c int) (d int))

(defar rocksdb-option-set-block-based-table-factory void
  (opt (* rocksdb-options)) (table-opts (* rocksdb-block-based-table-options)))

(define-opt-accessor rocksdb-options comparator (* rocksdb-comparator))
(defar rocksdb-options-set-merge-operator void
  (opt (* rocksdb-options))
  (merge-op (* rocksdb-mergeoperator)))
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
(defar rocksdb-options-set-db-log-dir void
  (opts (* rocksdb-options))
  (dir c-string))
(defar rocksdb-options-set-wal-dir void
  (opts (* rocksdb-options))
  (dir c-string))
(define-opt-accessor rocksdb-options wal-ttl-seconds unsigned-long)
(define-opt-accessor rocksdb-options wal-size-limit-mb unsigned-long)
(define-opt-accessor rocksdb-options manifest-preallocation-size size-t)
(define-opt-accessor rocksdb-options stats-dump-period-sec unsigned-int)
(define-opt-accessor rocksdb-options stats-persist-period-sec unsigned-int)

(define-opt-accessor rocksdb-options use-adaptive-mutex)
(define-opt-accessor rocksdb-options bytes-per-sync unsigned-long)
(define-opt-accessor rocksdb-options wal-bytes-per-sync unsigned-long)
(define-opt-accessor rocksdb-options file-max-buffer-size unsigned-long)
(define-opt-accessor rocksdb-options allow-concurrent-memtable-write)
(define-opt-accessor rocksdb-options enable-write-thread-adaptive-yield)
(define-opt-accessor rocksdb-options max-sequential-skip-in-iterations unsigned-long)
(define-opt-accessor rocksdb-options disable-auto-compactions)
(define-opt-accessor rocksdb-options optimize-filters-for-hits)
(define-opt-accessor rocksdb-options delete-obsolete-files-period-micros unsigned-long)
(define-opt-accessor rocksdb-options memtable-prefix-bloom-size-ratio double)
(define-opt-accessor rocksdb-options max-compaction-bytes unsigned-long)
(define-opt-accessor rocksdb-options memtable-huge-page-size size-t)
(define-opt-accessor rocksdb-options max-successive-merges size-t)
(define-opt-accessor rocksdb-options bloom-locality unsigned-int)
(define-opt-accessor rocksdb-options report-bg-io-stats)
(define-opt-accessor rocksdb-options experimental-mempurge-threshold double)
(define-opt-accessor rocksdb-options wal-recovery-mode int)
(define-opt-accessor rocksdb-options compression-options-parallel-threads int)
(define-opt-accessor rocksdb-options compression int)
(define-opt-accessor rocksdb-options bottommost-compression int)
(define-opt-accessor rocksdb-options compaction-style int)
(define-opt-accessor rocksdb-options wal-compression int)
#|
rocksdb_k_by_compensated_size_compaction_pri = 0,
rocksdb_k_oldest_largest_seq_first_compaction_pri = 1,
rocksdb_k_oldest_smallest_seq_first_compaction_pri = 2,
rocksdb_k_min_overlapping_ratio_compaction_pri = 3,
rocksdb_k_round_robin_compaction_pri = 4
|#
(define-opt-accessor rocksdb-options compaction-pri int)
;; (hash-link-list-rep)
;; (hash-skip-list-rep)
;; (memtable-vector-rep)

(defar rocksdb-options-set-row-cache void
  (opt (* rocksdb-options))
  (cache (* rocksdb-cache)))

(defar rocksdb-options-set-ratelimiter void
  (opt (* rocksdb-options))
  (limiter (* rocksdb-ratelimiter)))

(defar rocksdb-options-set-universal-compaction-options void
  (opt (* rocksdb-options))
  (opts (* rocksdb-universal-compaction-options)))

(defar rocksdb-options-set-min-level-to-compress void
  (opt (* rocksdb-options))
  (level int))

(defar rocksdb-options-set-plain-table-factory void
  (opt (* rocksdb-options))
  (i int)
  (d double)
  (s1 size-t)
  (s2 size-t)
  (c char)
  (f1 unsigned-char)
  (f2 unsigned-char))

(defar rocksdb-options-prepare-for-bulk-load void
  (opts (* rocksdb-options)))

(defar rocksdb-options-increase-parallelism void 
  (opt (* rocksdb-options)) (total-threads int))

(defar rocksdb-options-optimize-level-style-compaction void 
  (opt (* rocksdb-options))
  (memtable-memory-budget unsigned-long))

(defar rocksdb-options-optimize-universal-style-compaction void 
  (opt (* rocksdb-options))
  (memtable-memory-budget unsigned-long))

(defar rocksdb-options-enable-statistics void
  (opt (* rocksdb-options)))

(defar rocksdb-options-statistics-get-string c-string
  (opt (* rocksdb-options)))

(defar rocksdb-options-statistics-get-ticker-count unsigned-long
  (opt (* rocksdb-options))
  (ticker-type unsigned-int))

(defar rocksdb-options-statistics-get-histogram-data void
  (opt (* rocksdb-options))
  (histogram-type unsigned-int)
  (data (* rocksdb-statistics-histogram-data)))

(defar rocksdb-options-set-db-paths void
  (opt (* rocksdb-options))
  (paths (array (* rocksdb-dbpath)))
  (num-paths size-t))

(defar rocksdb-options-set-cf-paths void
  (opt (* rocksdb-options))
  (paths (array (* rocksdb-dbpath)))
  (num-paths size-t))

(defar rocksdb-options-set-env void
  (opts (* rocksdb-options))
  (env (* rocksdb-env)))

(defar rocksdb-options-set-info-log void
  (opts (* rocksdb-options))
  (logger (* rocksdb-logger)))

(defar rocksdb-options-set-uint64add-merge-operator void
  (opt (* rocksdb-options)))

(defar rocksdb-options-set-compression-per-level void
  (opt (* rocksdb-options))
  (levels (array int))
  (num-levels size-t))

(defar rocksdb-options-set-prefix-extractor void
  (self (* rocksdb-options))
  (val (* rocksdb-slicetransform)))

(defar rocksdb-options-set-sst-file-manager void
  (opt (* rocksdb-options))
  (sfm (* rocksdb-sst-file-manager)))

(defar rocksdb-options-add-compact-on-deletion-collector-factory-min-file-size void
  (opt (* rocksdb-options))
  (window-size size-t)
  (num-dels-trigger size-t)
  (deletion-ration double)
  (min-file-size unsigned-long))

;;; RocksDB Write Options
(define-opt rocksdb-writeoptions)
(define-opt-accessor rocksdb-writeoptions sync)
(define-opt-accessor rocksdb-writeoptions disable-wal)
(define-opt-accessor rocksdb-writeoptions ignore-missing-column-families)
(define-opt-accessor rocksdb-writeoptions no-slowdown)
(define-opt-accessor rocksdb-writeoptions low-pri)
(define-opt-accessor rocksdb-writeoptions memtable-insert-hint-per-batch)
;;; RocksDB Read Options
(define-opt rocksdb-readoptions)
(define-opt-accessor rocksdb-readoptions verify-checksums)
(define-opt-accessor rocksdb-readoptions fill-cache)
(define-opt-accessor rocksdb-readoptions read-tier int)
(define-opt-accessor rocksdb-readoptions tailing)
(define-opt-accessor rocksdb-readoptions total-order-seek)
(define-opt-accessor rocksdb-readoptions skippable-internal-keys unsigned-long)
(define-opt-accessor rocksdb-readoptions purge-on-iterator-cleanup)
(define-opt-accessor rocksdb-readoptions deadline unsigned-long)
(define-opt-accessor rocksdb-readoptions io-timeout unsigned-long)
(define-opt-accessor rocksdb-readoptions async-io)
(define-opt-accessor rocksdb-readoptions optimize-multiget-for-io)
(defar rocksdb-readoptions-set-snapshot void
  (self (* rocksdb-readoptions))
  (val (* rocksdb-snapshot)))
(defar rocksdb-readoptions-set-iterate-upper-bound void
  (self (* rocksdb-readoptions))
  (key (* char))
  (keylen size-t))
(defar rocksdb-readoptions-set-iterate-lower-bound void
  (self (* rocksdb-readoptions))
  (key (* char))
  (keylen size-t))

(defar rocksdb-readoptions-set-readahead-size void
  (self (* rocksdb-readoptions))
  (val size-t))

(defar rocksdb-readoptions-set-prefix-same-as-start void
  (self (* rocksdb-readoptions))
  (val unsigned-char))

(defar rocksdb-readoptions-set-ignore-range-deletions void
  (self (* rocksdb-readoptions))
  (val unsigned-char))

(defar rocksdb-readoptions-set-timestamp void
  (self (* rocksdb-readoptions))
  (ts (* char))
  (tslen size-t))

(defar rocksdb-readoptions-set-iter-start-ts void
  (self (* rocksdb-readoptions))
  (ts (* char))
  (tslen size-t))

(defar rocksdb-readoptions-set-auto-readahead-size void
  (self (* rocksdb-readoptions))
  (val unsigned-char))

;;; RocksDB Flush Options
(define-opt rocksdb-flushoptions)
(define-opt-accessor rocksdb-flushoptions wait)
;;; RocksDB Compact Options
(define-alien-enum (rocksdb-compaction-type)
  :level 0
  :universal 1
  :fifo 2)

(define-alien-enum (rocksdb-compaction-pri)
  :compensated-size 0
  :oldest-largest 1
  :oldest-smallest 2
  :min-overlapping-ratio 3
  :round-robin 4)

(define-opt rocksdb-compactoptions)
(define-opt-accessor rocksdb-compactoptions exclusive-manual-compaction)
(define-opt-accessor rocksdb-compactoptions bottommost-level-compaction)
(define-opt-accessor rocksdb-compactoptions change-level)
(define-opt-accessor rocksdb-compactoptions target-level int)
(define-opt-accessor rocksdb-compactoptions target-path-id int)
(define-opt-accessor rocksdb-compactoptions allow-write-stall unsigned-char)
(define-opt-accessor rocksdb-compactoptions max-subcompactions int)

(define-opt rocksdb-fifo-compaction-options)
(defar rocksdb-options-set-fifo-compaction-options void
  (opt (* rocksdb-options))
  (opts (* rocksdb-fifo-compaction-options)))
(define-opt-accessor rocksdb-fifo-compaction-options allow-compaction unsigned-char)
(define-opt-accessor rocksdb-fifo-compaction-options max-table-files-size unsigned-long)
(define-opt-accessor rocksdb-fifo-compaction-options max-data-files-size unsigned-long)
(define-opt-accessor rocksdb-fifo-compaction-options use-kv-ratio-compaction unsigned-char)

;;; RocksDB LRU Cache Options
(define-opt rocksdb-lru-cache-options)

(defar rocksdb-lru-cache-options-set-capacity void
  (self (* rocksdb-lru-cache-options))
  (val size-t))

(defar rocksdb-lru-cache-options-set-num-shard-bits void
  (self (* rocksdb-lru-cache-options))
  (val int))

(defar rocksdb-lru-cache-options-set-memory-allocator void
  (self (* rocksdb-lru-cache-options))
  (val (* rocksdb-memory-allocator)))

(export-opt-accessors rocksdb-lru-cache-options
                      capacity
                      num-shard-bits
                      memory-allocator)

#|
Load the latest rocksdb options from the specified db_path.

On success, num_column_families will be updated with a non-zero
number indicating the number of column families.
The returned db_options, column_family_names, and column_family_options
should be released via rocksdb_load_latest_options_destroy().

On error, a non-null errptr that includes the error message will be
returned.  db_options, column_family_names, and column_family_options
will be set to NULL.
|#
(def-with-errptr rocksdb-load-latest-options 
  void
  (db-path c-string)
  (env (* rocksdb-env))
  (ignore-unknown-options boolean)
  (cache (* rocksdb-cache))
  (db-options (* (* rocksdb-options)))
  (num-column-families (* size-t))
  (column-family-names (* (* (* char))))
  (column-family-options (* (* (* rocksdb-options)))))

(defar rocksdb-load-latest-options-destroy void
  (db-options (* rocksdb-options))
  (list-column-family-names (* c-string))
  (list-column-family-options (* (* rocksdb-options)))
  (len size-t))

(def-with-errptr rocksdb-set-options void
  (db (* rocksdb))
  (count int)
  (keys (array c-string))
  (values (array c-string)))

(def-with-errptr rocksdb-set-options-cf 
  void
  (db (* rocksdb))
  (handle (* rocksdb-column-family-handle))
  (count int)
  (keys (array (array unsigned-char)))
  (values (array (array unsigned-char))))

(defar rocksdb-options-create-copy (* rocksdb-options)
  (src (* rocksdb-options)))

;;; Aliases
;; some of the RocksDB options don't follow the standard naming
;; convention of 'rocksdb-*-set-*' and 'rocksdb-*-get-*'. In order to
;; remove the need for special-case handling in the high-level
;; interface we define them as aliases
(setf (symbol-function 'rocksdb-options-set-parallelism) #'rocksdb-options-increase-parallelism)

(declaim (inline rocksdb-options-set-enable-statistics rocksdb-options-set-prepare-for-bulk-load))
(defun rocksdb-options-set-enable-statistics (opt x)
  (when x
    (rocksdb-options-enable-statistics opt)))

(defun rocksdb-options-set-prepare-for-bulk-load (opt x)
  (when x
    (rocksdb-options-prepare-for-bulk-load opt)))
