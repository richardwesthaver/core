(in-package :rocksdb)

(defvar *rocksdb-compression-backends* #(snappy zlib bz2 lz4 lz4hc xpress zstd))

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
         skip-stats-update-on-db-open skip-checking-sst-file-sizes-on-db-open enable-blob-files
         min-blob-size blob-file-size blob-compression-type enable-blob-gc blob-gc-age-cutoff
         blob-gc-force-threshold blob-compaction-readahead-size blob-file-starting-level
         max-write-buffer-number min-write-buffer-number-to-merge max-write-buffer-number-to-maintain
         max-write-buffer-size-to-maintain enable-pipelined-write unordered-write max-subcompactions
         max-background-jobs max-background-compactions max-background-flushes max-log-file-size
         log-file-time-to-roll keep-log-file-num recycle-log-file-num soft-pending-compaction-bytes-limit
         hard-pending-compaction-bytes-limit max-manifest-file-size table-cache-numshardbits arena-block-size
         use-fsync db-log-dir wal-dir wal-ttl-seconds wal-size-limit-mb manifest-preallocation-size allow-mmap-reads
         allow-mmap-write use-direct-reads use-direct-io-for-flush-compaction is-fd-close-on-exec
         stats-dump-period-sec stas-persist-period-sec advise-random-on-open access-hint-on-compaction-start
         use-adaptive-mutex bytes-per-sync wal-bytes-per-sync writable-file-max-buffer-size
         allow-concurrent-memtable-write enable-write-thread-adaptive-yield max-sequential-skip-in-iterations
         disable-auto-compactions optimize-filters-for-hits delete-obsolete-files-period-micros
         prepare-for-bulk-load memtable-vector-rep memtable-prefix-bloom-size-ratio max-compaction-bytes
         hash-skip-list-rep plain-table-factory min-level-to-compress memtable-huge-page-size
         max-successive-merges bloom-locality inplace-update-support inplace-update-num-locks
         report-bg-io-stats avoid-unnecessary-blocking-io experimental-mempurge-threshold
         wal-recovery-mode compression bottommost-compression compaction-style universal-compaction-options
         ratelimiter atomic-flush row-cache manual-wal-flush wal-compression
         prepopulate-blob-cache))
  "Provides early list of options for macros to populate.")
