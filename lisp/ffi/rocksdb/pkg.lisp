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
  (:use :cl :std/base :std/alien :std/fu :std/sym)
  (:import-from :std/fu :symb)
  (:export
   :load-rocksdb
   ;; ERR
   :rocksdb-errptr
   ;; DB
   :rocksdb
   :rocksdb-open
   :rocksdb-close
   :rocksdb-destroy-db
   :rocksdb-repair-db
   :rocksdb-checkpoint-object-create
   :rocksdb-checkpoint-create
   :rocksdb-open-column-families
   :rocksdb-list-column-families-destroy
   :rocksdb-list-column-families
   :rocksdb-create-column-family
   :rocksdb-create-column-families
   :rocksdb-create-column-families-destroy
   :rocksdb-drop-column-family
   :rocksdb-column-family-handle-destroy
   :rocksdb-column-family-handle-get-id
   :rocksdb-column-family-handle-get-name
   :rocksdb-put
   :rocksdb-put-cf
   :rocksdb-get
   :rocksdb-get-with-ts
   :rocksdb-get-cf
   :rocksdb-get-cf-with-ts
   :rocksdb-merge
   :rocksdb-merge-cf
   :rocksdb-write
   :rocksdb-delete
   :rocksdb-delete-cf
   :rocksdb-delete-range-cf
   :rocksdb-multi-get
   :rocksdb-multi-get-with-ts
   :rocksdb-multi-get-cf
   :rocksdb-multi-get-cf-with-ts
   :rocksdb-batched-multi-get-cf
   :rocksdb-cancel-all-background-work
   ;; writebatch
   :rocksdb-writebatch-create
   :rocksdb-writebatch-create-from
   :rocksdb-writebatch-destroy
   :rocksdb-writebatch-clear
   :rocksdb-writebatch-count
   :rocksdb-writebatch-put
   :rocksdb-writebatch-put-cf
   :rocksdb-writebatch-put-cf-with-ts
   :rocksdb-writebatch-putv
   :rocksdb-writebatch-putv-cf
   :rocksdb-writebatch-merge
   :rocksdb-writebatch-mergev-cf
   :rocksdb-writebatch-delete
   :rocksdb-writebatch-delete-cf
   :rocksdb-writebatch-delete-cf-with-ts
   :rocksdb-writebatch-singledelete-cf
   :rocksdb-writebatch-singledelete-cf-with-ts
   :rocksdb-writebatch-deletev
   :rocksdb-writebatch-deletev-cf
   :rocksdb-writebatch-deletev-cf-with-ts
   :rocksdb-writebatch-delete-range
   :rocksdb-writebatch-delete-range-cf
   :rocksdb-writebatch-delete-rangev
   :rocksdb-writebatch-delete-rangev-cf
   :rocksdb-writebatch-put-log-data
   :rocksdb-writebatch-iterate
   :rocksdb-writebatch-data
   :rocksdb-writebatch-set-save-point
   :rocksdb-writebatch-rollback-to-save-point
   :rocksdb-writebatch-pop-save-point
   ;; flush
   :rocksdb-flush
   :rocksdb-flushoptions-create
   :rocksdb-flushoptions-destroy
   :rocksdb-flushoptions-get-wait
   :rocksdb-flushoptions-set-wait
   :rocksdb-flush-cf
   :rocksdb-flush-cfs
   :rocksdb-flush-wal
   ;; CACHE
   :rocksdb-cache
   :rocksdb-cache-create-lru
   ;; BLOCK-BASED OPTIONS
   :rocksdb-block-based-table-options
   :rocksdb-block-based-options-create
   :rocksdb-block-based-options-destroy
   :rocksdb-block-based-options-set-block-cache
   ;; OPTIONS
   ;; opt-utils
   :rocksdb-load-latest-options
   :rocksdb-load-latest-options-destroy
   :rocksdb-set-options
   :rocksdb-set-options-cf
   :rocksdb-options
   :rocksdb-options-create
   :rocksdb-options-destroy
   :rocksdb-options-increase-parallelism
   :rocksdb-options-optimize-for-point-lookup
   :rocksdb-options-optimize-level-style-compaction
   :rocksdb-options-optimize-universal-style-compaction
   :rocksdb-options-set-allow-ingest-behind
   :rocksdb-options-get-allow-ingest-behind
   :rocksdb-options-set-compaction-filter
   :rocksdb-options-set-compaction-filter-factory
   :rocksdb-options-compaction-readahead-size
   :rocksdb-options-get-compaction-readahead-size
   :rocksdb-options-set-comparator
   :rocksdb-options-set-merge-operator
   :rocksdb-options-set-uint64add-merge-operator
   :rocksdb-options-set-compression-per-level
   :rocksdb-options-set-create-if-missing
   :rocksdb-options-get-create-if-missing
   :rocksdb-options-set-create-missing-column-families
   :rocksdb-options-get-create-missing-column-families
   :rocksdb-options-set-error-if-exists
   :rocksdb-options-get-error-if-exists
   :rocksdb-options-set-paranoid-checks
   :rocksdb-options-get-paranoid-checks
   :rocksdb-options-set-db-paths
   :rocksdb-options-set-env
   :rocksdb-options-set-info-log
   :rocksdb-options-set-info-log-level
   :rocksdb-options-get-info-log-level
   :rocksdb-options-set-write-buffer-size
   :rocksdb-options-get-write-buffer-size
   :rocksdb-options-set-db-write-buffer-size
   :rocksdb-options-get-db-write-buffer-size
   :rocksdb-options-set-max-open-files
   :rocksdb-options-get-max-open-files
   :rocksdb-options-set-max-total-wal-size
   :rocksdb-options-get-max-total-wal-size
   :rocksdb-options-set-compression-options
   :rocksdb-options-set-compression-options-zstd-max-train-bytes
   :rocksdb-options-get-compression-options-zstd-max-train-bytes
   :rocksdb-options-set-compression-options-use-zstd-dict-trainer
   :rocksdb-options-get-compression-options-use-zstd-dict-trainer
   :rocksdb-options-set-compression-options-parallel-threads
   :rocksdb-options-get-compression-options-parallel-threads
   :rocksdb-options-set-compression-options-max-dict-buffer-bytes
   :rocksdb-options-get-compression-options-max-dict-buffer-bytes
   :rocksdb-options-set-block-based-table-factory
   ;; blob
   :rocksdb-options-set-enable-blob-files
   :rocksdb-options-get-enable-blob-files
   :rocksdb-options-set-min-blob-size
   :rocksdb-options-set-blob-file-size
   :rocksdb-options-set-blob-compression-type
   :rocksdb-options-get-blob-compression-type
   :rocksdb-options-set-enable-blob-gc
   :rocksdb-options-get-enable-blob-gc
   :rocksdb-options-set-blob-gc-age-cutoff
   :rocksdb-options-get-blob-gc-age-cutoff
   :rocksdb-options-set-blob-gc-force-threshold
   :rocksdb-options-get-blob-gc-force-threshold
   :rocksdb-options-set-blob-compaction-readahead-size
   :rocksdb-options-get-blob-compaction-readahead-size
   :rocksdb-options-set-blob-file-starting-level
   :rocksdb-options-set-blob-cache
   :rocksdb-options-set-prepopulate-blob-cache
   :rocksdb-options-get-prepopulate-blob-cache
   ;; read
   :rocksdb-readoptions
   :rocksdb-readoptions-create
   :rocksdb-readoptions-destroy
   ;; write
   :rocksdb-writeoptions
   :rocksdb-writeoptions-create
   :rocksdb-writeoptions-destroy
   ;; compact
   :rocksdb-compactoptions
   :rocksdb-compact-range-cf
   :rocksdb-suggest-compact-range
   :rocksdb-suggest-compact-range-cf
   :rocksdb-compact-range-opt
   :rocksdb-compact-range-cf-opt
   ;; ITERATOR
   :rocksdb-iterator
   :rocksdb-iter-seek-to-first
   :rocksdb-iter-seek-to-last
   :rocksdb-iter-seek
   :rocksdb-iter-seek-for-prev
   :rocksdb-iter-next
   :rocksdb-iter-prev
   :rocksdb-create-iterator
   :rocksdb-iter-key
   :rocksdb-iter-value
   :rocksdb-iter-timestamp
   :rocksdb-iter-destroy
   :rocksdb-iter-valid
   :rocksdb-iter-get-error   
   :rocksdb-get-updates-since
   :rocksdb-create-iterator-cf
   :rocksdb-create-iterators
   :rocksdb-create-snapshot
   :rocksdb-release-snapshot
   :rocksdb-property-value
   :rocksdb-property-int
   :rocksdb-property-int-cf
   :rocksdb-property-value-cf
   :rocksdb-approximate-sizes
   :rocksdb-approximate-sizes-cf
   :rocksdb-wal-iter-next
   :rocksdb-wal-iter-valid
   :rocksdb-wal-iter-status
   :rocksdb-wal-iter-get-batch
   :rocksdb-wal-iter-destroy
   :rocksdb-backup-engine
   :rocksdb-backup-engine-info
   :rocksdb-backup-engine-options
   :rocksdb-backup-engine-open
   :rocksdb-backup-engine-create-new-backup
   :rocksdb-backup-engine-restore-db-from-backup
   :rocksdb-backup-engine-restore-db-from-latest-backup
   :rocksdb-backup-engine-close
   ;; transactions
   :rocksdb-transaction
   :rocksdb-transaction-options
   :rocksdb-transactiondb
   :rocksdb-transactiondb-options
   :rocksdb-optimistictransactiondb
   :rocksdb-optimistictransactiondb-options
   :rocksdb-transactiondb-open
   :rocksdb-transactiondb-close
   :rocksdb-transaction-begin))

(in-package :rocksdb)

(defun load-rocksdb () 
  (unless (member :rocksdb *features*)
    (sb-alien:load-shared-object "librocksdb.so" :dont-save t)
    (push :rocksdb *features*)))
