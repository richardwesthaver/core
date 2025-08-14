;;; rocksdb/vars.lisp --- RocksDB FFI Variables

;;

;;; Code:
(in-package :rocksdb)

(defmacro %svec (&body syms)
  `(map 'vector 
        (lambda (x) (string-downcase (symbol-name x)))
        ',@(list syms)))

;;; Callbacks
(defvar *rocksdb-destructor-callback* (alien-callable-function 'rocksdb-destructor))
(defvar *rocksdb-delete-value-callback* (alien-callable-function 'rocksdb-delete-value))
(defvar *rocksdb-name-callback* (alien-callable-function 'rocksdb-name))
(defvar *rocksdb-log-callback* (alien-callable-function 'rocksdb-log-default))

;;; Opts
(defvar *rocksdb-compression-backends*
  (%svec none snappy zlib bz2 lz4 lz4hc xpress zstd))

(defun rocksdb-compression-backend (name)
  (position name *rocksdb-compression-backends* :test #'string=))

(defvar *rocksdb-compaction-levels* '(level universal fifo))

(defun rocksdb-compaction-level (name)
  (position name *rocksdb-compaction-levels* :test #'string=))

(defvar *rocksdb-perf-metrics*
  (%svec
    user-key-comparison-count block-cache-hit-count
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

(defvar *rocksdb-perf-levels*
  (%svec uninitialized disable enable-count enable-time-except-for-mutex enable-time out-of-bounds))

(defun rocksdb-perf-level (name)
  (position name *rocksdb-perf-levels* :test #'string=))

(defvar *rocksdb-statistics-levels*
  (%svec disable-all except-tickers except-histogram-or-timers
    except-timers except-detailed-timers except-time-for-mutex
    all))

(defun rocksdb-statistics-level (name)
  (position name *rocksdb-statistics-levels* :test #'string=))

(defvar *rocksdb-set-only-options*
  (%svec block-based-table-factory parallelism compression-options
    merge-operator db-log-dir wal-dir wal-ttl-seconds wal-size-limit-mb
    memtable-vector-rep prepare-for-bulk-load universal-compaction-options
    hash-skip-list-rep plain-table-factory min-level-to-compress
    ratelimiter row-cache prefix-extractor))

(defvar *rocksdb-options*
  (%svec create-if-missing create-missing-column-families error-if-exists
    paranoid-checks info-log-level write-buffer-size db-write-buffer-size
    max-open-files max-file-opening-threads max-total-wal-size
    compression-options-zstd-max-train-bytes compression-options-max-dict-buffer-bytes
    compression-options-parallel-threads compression-options-use-zstd-dict-trainer
    num-levels level0-file-num-compaction-trigger level0-slowdown-writes-trigger
    level0-stop-writes-trigger target-file-size-base target-file-size-multiplier 
    max-bytes-for-level-base level-compaction-dynamic-level-bytes max-bytes-for-level-multiplier
    compaction-pri
    allow-ingest-behind statistics-level
    skip-stats-update-on-db-open skip-checking-sst-file-sizes-on-db-open enable-blob-files
    min-blob-size blob-file-size blob-compression-type enable-blob-gc blob-gc-age-cutoff
    blob-gc-force-threshold blob-compaction-readahead-size blob-file-starting-level
    ;; deprecated: max-write-buffer-number-to-maintain
    max-write-buffer-number min-write-buffer-number-to-merge memtable-op-scan-flush-trigger
    max-write-buffer-size-to-maintain enable-pipelined-write unordered-write max-subcompactions
    max-background-jobs max-background-compactions max-background-flushes max-log-file-size
    log-file-time-to-roll keep-log-file-num recycle-log-file-num soft-pending-compaction-bytes-limit
    hard-pending-compaction-bytes-limit max-manifest-file-size table-cache-numshardbits arena-block-size
    use-fsync manifest-preallocation-size allow-mmap-reads
    allow-mmap-writes use-direct-reads use-direct-io-for-flush-and-compaction is-fd-close-on-exec
    stats-dump-period-sec stats-persist-period-sec advise-random-on-open
    use-adaptive-mutex bytes-per-sync wal-bytes-per-sync writable-file-max-buffer-size
    allow-concurrent-memtable-write enable-write-thread-adaptive-yield max-sequential-skip-in-iterations
    disable-auto-compactions optimize-filters-for-hits delete-obsolete-files-period-micros
    memtable-prefix-bloom-size-ratio max-compaction-bytes
    memtable-huge-page-size
    max-successive-merges bloom-locality inplace-update-support inplace-update-num-locks
    report-bg-io-stats avoid-unnecessary-blocking-io experimental-mempurge-threshold
    wal-recovery-mode compression bottommost-compression compaction-style 
    atomic-flush manual-wal-flush wal-compression
    prepopulate-blob-cache)
  "Provides early list of options for macros to populate.")

(defvar *rocksdb-set-only-readoptions*
  (%svec snapshot iterate-upper-bound iterate-lower-bound readahead-size
    prefix-same-as-start ignore-range-deletions timestamp iter-start-ts auto-readahead-size))

(defvar *rocksdb-readoptions*
  (%svec verify-checksums fill-cache read-tier tailing total-order-seek skippable-internal-keys
    purge-on-iterator-cleanup deadline io-timeout async-io))

(defvar *rocksdb-writeoptions*
  (%svec sync disable-wal ignore-missing-column-families
    no-slowdown low-pri memtable-insert-hint-per-batch))

(defvar *rocksdb-flushoptions* (%svec wait))
(defvar *rocksdb-lru-cache-options* '(capacity num-shard-bits memory-allocator))
(defvar *rocksdb-compactoptions* 
  (%svec exclusive-manual-compaction 
         bottommost-level-compaction 
         change-level
         target-level
         target-path-id
         allow-write-stall
         max-subcompactions))

(defvar *rocksdb-ingestexternalfileoptions*
  (%svec move-files snapshot-consistency allow-global-seqno allow-blocking-flush 
    ingest-behind fail-if-not-bottommost-level))
(defvar *rocksdb-set-only-backup-engine-options* (%svec backup-dir env))
(defvar *rocksdb-backup-engine-options*
  (%svec share-table-files sync destroy-old-data backup-log-files backup-rate-limit
    restore-rate-limit callback-trigger-interval-size max-valid-backups-to-open
    shared-files-with-checksum-naming))

;; (defvar *rocksdb-intgestexternalfileoptions*)

(defvar *rocksdb-column-family-metadata*
  #("size" "file-count" "name" "level-count" "level-metadata"))

(defvar *rocksdb-level-metadata*
  #("level" "size" "file-count" "sst-file-metadata"))

(defvar *rocksdb-sst-file-metadata*
  #("relative-filename" "directory" "size" "smallestkey" "largestkey"))

(defvar *rocksdb-properties*
  #("rocksdb.num-files-at-level0"
    "rocksdb.compression-ration-at-level0"
    "rocksdb.aggregated-table-properties-at-level0"    
    "rocksdb.stats" "rocksdb.sstables"
    "rocksdb.cfstats" "rocksdb.cfstats-no-file-histogram"
    "rocksdb.cf-file-histogram" "rocksdb.cf-write-stall-stats"
    "rocksdb.db-write-stall-stats" "rocksdb.dbstats"
    "rocksdb.levelstats" "rocksdb.block-cache-entry-stats"
    "rocksdb.fast-block-cache-entry-stats" "rocksdb.num-immutable-mem-table"
    "rocksdb.num-immutable-mem-table-flushed" "rocksdb.mem-table-flush-pending"
    "rocksdb.num-running-flushes" "rocksdb.compaction-pending"
    "rocksdb.num-running-compactions" "rocksdb.background-errors"
    "rocksdb.cur-size-active-mem-table" "rocksdb.cur-size-all-mem-tables"
    "rocksdb.size-all-mem-tables" "rocksdb.num-entries-active-mem-table"
    "rocksdb.num-entries-imm-mem-tables" "rocksdb.num-deletes-active-mem-table"
    "rocksdb.num-deletes-imm-mem-tables" "rocksdb.estimate-num-keys"
    "rocksdb.estimate-table-readers-mem" "rocksdb.is-file-deletions-enabled"
    "rocksdb.num-snapshots" "rocksdb.oldest-snapshot-time"
    "rocksdb.oldest-snapshot-sequence" "rocksdb.num-live-versions"
    "rocksdb.current-super-version-number" "rocksdb.estimate-live-data-size"
    "rocksdb.min-log-number-to-keep" "rocksdb.min-obsolete-sst-number-to-keep"
    "rocksdb.total-sst-files-size" "rocksdb.live-sst-files-size"
    "rocksdb-obsolete-sst-files-size" "rocksdb.live-sst-files-size-at-temperature"
    "rocksdb.base-level" "rocksdb.estimate-pending-compaction-bytes"
    "rocksdb.aggregated-table-properties" "rocksdb.actual-delayed-write-rate"
    "rocksdb.is-write-stopped" "rocksdb.estimate-oldest-key-time"
    "rocksdb.block-cache-capacity" "rocksdb.block-cache-pinned-usage"
    "rocksdb.options-statistics" "rocksdb-num-blob-files"
    "rocksdb.blob-stats" "rocksdb.total-blob-file-size"
    "rocksdb.live-blob-file-size" "rocksdb.live-blob-file-garbage-size"
    "rocksdb.blob-cache-capacity" "rocksdb.blob-cache-usage"
    "rocksdb.blob-cache-pinned-usage")
  "Vector of unique property prefixes for use with ROCKSDB-PROPERTY-VALUE.")

(defun rocksdb-num-files-at-level (n)
  (format nil "rocksdb.num-files-at-level~A" n))

(defun rocksdb-compression-ratio-at-level (n)
  (format nil "rocksdb.compression-ratio-at-level~A" n))

(defun rocksdb-aggregated-table-properties-at-level (n)
  (format nil "rocksdb.aggregated-table-properties-at-level~A" n))
