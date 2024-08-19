;;; rocksdb.lisp --- low-level bindings to the RocksDB C API

;; for the high-level interface, see rdb.lisp.

;;; Commentary:

;; if ur on archlinux and installed rocksdb via AUR you may receive an error from
;; jemalloc: cannot allocate memory in static TLS block:

;; https://github.com/veer66/cl-rocksdb/issues/1

;; for best results, you should compile rocksdb from source - use j0ni's snippet as a
;; starting point.

;; make shared_lib DISABLE_JEMALLOC=1 && 
;; sudo cp librocksdb.so.* /usr/local/lib/ && 
;; sudo cp -rf include/* /usr/local/include/

;; https://github.com/facebook/rocksdb/blob/main/Makefile

;; check /usr/local/include/rocksdb/c.h for the C API header, the source is under
;; db/c.cc

;; here are some important notes to keepin mind (from the API header):
#|
C bindings for rocksdb.  May be useful as a stable ABI that can be
used by programs that keep rocksdb in a shared library, or for
a JNI api.

Does not support:
. getters for the option types
. custom comparators that implement key shortening
. capturing post-write-snapshot
. custom iter, db, env, cache implementations using just the C bindings

Some conventions:

(1) We expose just opaque struct pointers and functions to clients.
This allows us to change internal representations without having to
recompile clients.

(2) For simplicity, there is no equivalent to the Slice type.  Instead,
the caller has to pass the pointer and length as separate
arguments.

(3) Errors are represented by a null-terminated c string.  NULL
means no error.  All operations that can raise an error are passed
a "char** errptr" as the last argument.  One of the following must
be true on entry:
*errptr == NULL
*errptr points to a malloc()ed null-terminated error message
On success, a leveldb routine leaves *errptr unchanged.
On failure, leveldb frees the old value of *errptr and
set *errptr to a malloc()ed error message.

(4) Bools have the type unsigned char (0 == false; rest == true)

(5) All of the pointer arguments must be non-NULL.|#

;;; Code:
(defpackage :rocksdb
  (:use :cl :std/alien :std/sym :std/macs :sb-alien)
  (:export
   ;; vars
   :*rocksdb-options*
   :*rocksdb-compaction-levels*
   :*rocksdb-compression-backends*
   :rocksdb-compression-backend
   :*rocksdb-perf-metrics*
   :*rocksdb-perf-levels*
   :*rocksdb-statistics-levels*
   :rocksdb-statistics-level
   :rocksdb-perf-level
   :*rocksdb-column-family-metadata*
   :*rocksdb-level-metadata*
   :*rocksdb-sst-file-metadata*
   :*rocksdb-properties*
   :rocksdb-num-files-at-level
   :rocksdb-compression-ratio-at-level
   :rocksdb-aggregated-table-properties-at-level
   :rocksdb-concat-partial-merge
   :rocksdb-concat-full-merge
   :rocksdb-name
   :rocksdb-concat-delete-value
   :rocksdb-destructor
   :rocksdb-slicetransform-create
   :rocksdb-slicetransform-create-noop
   :rocksdb-slicetransform-destroy
   :rocksdb-slicetransform-create-fixed-prefix
   :rocksdb-ingestexternalfileoptions-set-move-files
   :rocksdb-ingestexternalfileoptions-set-snapshot-consistency
   :rocksdb-ingestexternalfileoptions-set-allow-global-seqno
   :rocksdb-ingestexternalfileoptions-set-allow-blocking-flush
   :rocksdb-ingestexternalfileoptions-set-ingest-behind
   :rocksdb-ingestexternalfileoptions-set-fail-if-not-bottommost-level
   :rocksdb-backup-engine-options-set-backup-dir
   :rocksdb-restore-options-set-keep-log-files
   :rocksdb-hyper-clock-cache-options-set-estimated-entry-charge
   :rocksdb-backup-engine-options-set-env
   :rocksdb-hyper-clock-cache-options-set-capacity
   :rocksdb-hyper-clock-cache-options-set-num-shard-bits
   :rocksdb-hyper-clock-cache-options-set-memory-allocator
   :rocksdb-wal-readoptions
   :rocksdb-block-based-options-create
   :rocksdb-block-based-options-set-top-level-index-pinning-tier
   :rocksdb-block-based-options-set-partition-pinning-tier
   :rocksdb-block-based-options-set-unpartition-pinning-tier
   :rocksdb-block-based-options-destroy
   :rocksdb-options-increase-parallelism
   :rocksdb-options-set-uint64add-merge-operator
   :rocksdb-options-enable-statistics
   :rocksdb-options-set-db-paths
   :rocksdb-options-set-env
   :rocksdb-options-statistics-get-ticker-count
   :rocksdb-options-set-plain-table-factory
   :rocksdb-options-prepare-for-bulk-load
   :rocksdb-options-set-ratelimiter
   :rocksdb-options-optimize-level-style-compaction
   :rocksdb-options-set-compression-per-level
   :rocksdb-options-statistics-get-string
   :rocksdb-options-set-cf-paths
   :rocksdb-options-set-info-log
   :rocksdb-options-statistics-get-histogram-data
   :rocksdb-options-set-min-level-to-compress
   :rocksdb-options-set-universal-compaction-options
   :rocksdb-options-set-row-cache
   :rocksdb-load-latest-options-destroy
   :rocksdb-options-create-copy
   :rocksdb-options-set-parallelism
   :rocksdb-options-set-prepare-for-bulk-load
   :rocksdb-options-set-enable-statistics
   :rocksdb-close
   :rocksdb-enable-manual-compaction
   :rocksdb-cancel-all-background-work
   :rocksdb-disable-manual-compaction
   :rocksdb-multi-get
   :rocksdb-multi-get-with-ts
   :rocksdb-multi-get-cf
   :rocksdb-multi-get-cf-with-ts
   :rocksdb-cache-create-lru
   :rocksdb-delete-file
   :rocksdb-livefile
   :rocksdb-property-value
   :rocksdb-property-value-cf
   :rocksdb-property-int
   :rocksdb-property-int-cf
   :rocksdb-create-column-families-destroy
   :rocksdb-column-family-handle-get-id
   :rocksdb-column-family-handle-destroy
   :rocksdb-column-family-handle-get-name
   :rocksdb-list-column-families-destroy
   :rocksdb-create-iterator
   :rocksdb-iter-seek-to-last
   :rocksdb-iter-next
   :rocksdb-iter-timestamp
   :rocksdb-wal-iter-get-batch
   :rocksdb-iter-destroy
   :rocksdb-iter-seek
   :rocksdb-iter-prev
   :rocksdb-iter-get-error
   :rocksdb-get-latest-sequence-number
   :rocksdb-iter-seek-to-first
   :rocksdb-iter-seek-for-prev
   :rocksdb-iter-value
   :rocksdb-wal-iter-next
   :rocksdb-wal-iter-destroy
   :rocksdb-iter-valid
   :rocksdb-create-iterator-cf
   :rocksdb-wal-iter-valid
   :rocksdb-backup-engine-close
   :rocksdb-transaction-begin
   :rocksdb-transactiondb-release-snapshot
   :rocksdb-transactiondb-property-int
   :rocksdb-transactiondb-get-close-db
   :rocksdb-transaction-set-savepoint
   :rocksdb-transaction-create-iterator
   :rocksdb-transactiondb-create-iterator
   :rocksdb-optimistictransactiondb-get-base-db
   :rocksdb-optimistictransaction-begin
   :rocksdb-transactiondb-flush-wal
   :rocksdb-transaction-close
   :rocksdb-transactiondb-property-value
   :rocksdb-transactiondb-get-base-db
   :rocksdb-transaction-get-name
   :rocksdb-transaction-destroy
   :rocksdb-transaction-create-iterator-cf
   :rocksdb-transactiondb-create-iterator-cf
   :rocksdb-optimistictransactiondb-close-base-db
   :rocksdb-optimistictransactiondb-close
   :rocksdb-transactiondb-create-snapshot
   :rocksdb-perfcontext-reset
   :rocksdb-perfcontext-metric
   :rocksdb-perfcontext-report
   :rocksdb-perfcontext-destroy
   :rocksdb-set-perf-level
   :rocksdb-filterpolicy-destroy
   :rocksdb-filterpolicy-create-ribbon
   :rocksdb-filterpolicy-create-bloom
   :rocksdb-filterpolicy-create-ribbon-hybrid
   :rocksdb-filterpolicy-create-bloom-full
   :rocksdb-create-snapshot
   :rocksdb-snapshot-get-sequence-number
   :rocksdb-release-snapshot
   :rocksdb-errptr
   :rocksdb-sstfilewriter-create
   :rocksdb-sstfilewriter-destroy
   :rocksdb-sstfilewriter-create-with-comparator
   :rocksdb-statistics-histogram-data-create
   :rocksdb-statistics-histogram-data-get-median
   :rocksdb-statistics-histogram-data-get-p99
   :rocksdb-statistics-histogram-data-get-std-dev
   :rocksdb-statistics-histogram-data-get-count
   :rocksdb-statistics-histogram-data-get-min
   :rocksdb-statistics-histogram-data-destroy
   :rocksdb-statistics-histogram-data-get-p95
   :rocksdb-statistics-histogram-data-get-average
   :rocksdb-statistics-histogram-data-get-max
   :rocksdb-statistics-histogram-data-get-sum
   :rocksdb-mergeoperator-create
   :rocksdb-full-merge-function
   :rocksdb-delete-value-function
   :rocksdb-mergeoperator-destroy
   :rocksdb-partial-merge-function
   :rocksdb-destructor-function
   :rocksdb-get-column-family-metadata
   :rocksdb-column-family-metadata-destroy
   :rocksdb-column-family-metadata-get-file-count
   :rocksdb-column-family-metadata-get-level-count
   :rocksdb-level-metadata-destroy
   :rocksdb-level-metadata-get-size
   :rocksdb-level-metadata-get-sst-file-metadata
   :rocksdb-sst-file-metadata-get-relative-filename
   :rocksdb-sst-file-metadata-get-size
   :rocksdb-sst-file-metadata-get-largestkey
   :rocksdb-get-column-family-metadata-cf
   :rocksdb-column-family-metadata-get-size
   :rocksdb-column-family-metadata-get-name
   :rocksdb-column-family-metadata-get-level-metadata
   :rocksdb-level-metadata-get-level
   :rocksdb-level-metadata-get-file-count
   :rocksdb-sst-file-metadata-destroy
   :rocksdb-sst-file-metadata-get-directory
   :rocksdb-sst-file-metadata-get-smallestkey
   :rocksdb-compactionfilter-set-ignore-snapshots
   :rocksdb-compactionfiltercontext-is-full-compaction
   :rocksdb-compactionfilter-destroy
   :rocksdb-compactionfiltercontext-is-manual-compaction
   :rocksdb-compactionfilter-create
   :rocksdb-comparator-destroy
   :rocksdb-comparator-create
   :rocksdb-comparator-with-ts-create
   :rocksdb-checkpoint-object-destroy
   :rocksdb-name-function
   :rocksdb-concat-merge-name
   :rocksdb-compare-never-without-ts
   :rocksdb-compare-never-with-ts
   :rocksdb-compare-never-name
   :rocksdb-compare-never
   :rocksdb-compare-without-ts-function
   :rocksdb-compare-with-ts-function
   :rocksdb-compare-function
   :rocksdb-create-compaction-filter
   :rocksdb-create-compaction-filter-function
   :rocksdb-get-default-column-family-handle
   :rocksdb-get-db-identity
   :rocksdb-batched-multi-get-cf
   :rocksdb-key-may-exist
   :rocksdb-key-may-exist-cf
   :rocksdb-backup-engine-get-backup-info
   :rocksdb-backup-engine-info-count
   :rocksdb-backup-engine-info-timestamp
   :rocksdb-backup-engine-info-backup-id
   :rocksdb-backup-engine-info-size
   :rocksdb-backup-engine-info-num-files
   :rocksdb-backup-engine-info-destroy
   :rocksdb-iter-key
   :rocksdb-get-updates-since
   :rocksdb-create-iterators
   :rocksdb-create-compaction-filter-never
   :rocksdb-compacitonfilterfactory-destroy
   :rocksdb-compactionfilterfactory-create
   :rocksdb-filter-never
   :rocksdb-delete-value))

(in-package :rocksdb)

(define-alien-loader "rocksdb" t)
