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
(defpackage :rocksdb/pkg
  (:nicknames :rocksdb)
  (:use :cl :std :rocksdb/macs)
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
   :rocksdb-wal-iter-destroy))

(in-package :rocksdb)

(defun load-rocksdb () 
  (unless (member :rocksdb *features*)
    (sb-alien:load-shared-object "librocksdb.so" :dont-save t)
    (push :rocksdb *features*)))

(load-rocksdb)

;;; Types
;; db types
(define-alien-type rocksdb (struct rocksdb-t))
(define-alien-type rocksdb-logger (struct rocksdb-logger-t))
(define-alien-type rocksdb-iterator (struct rocksdb-iterator-t))
(define-alien-type rocksdb-cache (struct rocksdb-cache-t))
(define-alien-type rocksdb-checkpoint (struct rocksdb-checkpoint-t))
(define-alien-type rocksdb-snapshot (struct rocksdb-snapshot-t))
(define-alien-type rocksdb-comparator (struct rocksdb-comparator-t))
(define-alien-type rocksdb-transaction (struct rocksdb-transaction-t))
(define-alien-type rocksdb-transactiondb (struct rocksdb-transactiondb-t))
(define-alien-type rocksdb-livefiles (struct rocksdb-column-family-livefiles-t))
(define-alien-type rocksdb-writebatch (struct rocksdb-column-family-writebatch-t))
(define-alien-type rocksdb-mergeoperator (struct rocksdb-mergeoperator-t))
(define-alien-type rocksdb-env (struct rocksdb-env-t))
;; column-family
(define-alien-type rocksdb-column-family-handle (struct rocksdb-column-family-handler-t))
(define-alien-type rocksdb-column-family-metadata (struct rocksdb-column-family-metadata-t))
;; options
(define-alien-type rocksdb-options (struct rocksdb-options-t))
(define-alien-type rocksdb-readoptions (struct rocksdb-readoptions-t))
(define-alien-type rocksdb-writeoptions (struct rocksdb-writeoptions-t))
(define-alien-type rocksdb-compactoptions (struct rocksdb-compactoptions-t))
(define-alien-type rocksdb-block-based-table-options (struct rocksdb-block-based-table-options-t))
(define-alien-type rocksdb-flushoptions (struct rocksdb-flushoptions-t))
(define-alien-type rocksdb-universal-compaction-options (struct rocksdb-universal-compaction-options-t))
(define-alien-type rocksdb-envoptions (struct rocksdb-envoptions-t))
;; stats
(define-alien-type rocksdb-statistics-histogram-data (struct rocksdb-statistics-histogram-data-t))
(define-alien-type rocksdb-memory-usage (struct rocksdb-memory-usage-t))
;; sst
(define-alien-type rocksdb-sstfilewriter (struct rocksdb-sstfilewriter-t))
(define-alien-type rocksdb-sst-file-metadata (struct rocksdb-sst-file-metadata-t))
;; wal
(define-alien-type rocksdb-wal-iterator (struct rocksdb-wal-iterator-t))
(define-alien-type rocksdb-wal-readoptions (struct rocksdb-wal-readoptions-t))
;; errors
;; either (* void) or c-string (* (* char))
(define-alien-type rocksdb-errptr (* (* t)))

;;; Cache
(define-alien-routine rocksdb-cache-create-lru (* rocksdb) (capacity unsigned-int))

;;; Options
(define-alien-routine rocksdb-load-latest-options void
  (db-path c-string)
  (env (* rocksdb-env))
  (ignore-unknown-options boolean)
  (cache (* rocksdb-cache))
  (db-options (* (* rocksdb-options)))
  (num-column-families (* size-t))
  (column-family-names (* (* c-string)))
  (column-family-options (* (* (* rocksdb-options))))
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-load-latest-options-destroy void
  (db-options (* (* rocksdb-options)))
  (list-column-family-names (* c-string))
  (list-column-family-options (* (* rocksdb-options)))
  (len size-t))

(define-alien-routine rocksdb-set-options void
  (db (* rocksdb))
  (count int)
  (keys (array c-string))
  (values (array c-string))
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-set-options-cf void
  (db (* rocksdb))
  (handle (* rocksdb-column-family-handle))
  (count int)
  (keys (array c-string))
  (values (array c-string))
  (errptr rocksdb-errptr))

;;;; bb-opts
(define-alien-routine rocksdb-block-based-options-create (* rocksdb-block-based-table-options))
(define-alien-routine rocksdb-block-based-options-destroy void 
  (options (* rocksdb-block-based-table-options)))
(define-alien-routine rocksdb-block-based-options-set-block-cache void 
  (options (* rocksdb-block-based-table-options)) 
  (block-cache (* rocksdb-cache)))
(define-alien-routine rocksdb-block-based-options-set-cache-index-and-filter-blocks void
  (options (* rocksdb-block-based-table-options))
  (val c-string))

;;;; db-opts
(define-alien-routine rocksdb-options-create (* rocksdb-options))
(define-alien-routine rocksdb-options-destroy void 
  (options (* rocksdb-options)))
(define-alien-routine rocksdb-options-increase-parallelism void 
  (opt (* rocksdb-options)) (total-threads int))
(define-alien-routine rocksdb-options-optimize-level-style-compaction void 
  (opt (* rocksdb-options))
  (memtable-memory-budget unsigned-long))
(define-alien-routine rocksdb-options-set-create-if-missing void 
  (opt (* rocksdb-options))
  (val boolean))
(define-alien-routine rocksdb-options-set-block-based-table-factory void
  (opt (* rocksdb-options))
  (table-options (* rocksdb-block-based-table-options)))
(define-alien-routine rocksdb-options-set-allow-ingest-behind void
  (opts (* rocksdb-options))
  (val unsigned-char))
(define-alien-routine rocksdb-options-set-merge-operator void
  (opts (* rocksdb-options))
  (comparator (* rocksdb-comparator)))

;;;; write-opts
(define-alien-routine rocksdb-writeoptions-create (* rocksdb-writeoptions))
(define-alien-routine rocksdb-writeoptions-destroy void
  (opt (* rocksdb-writeoptions)))
;;;; read-opts
(define-alien-routine rocksdb-readoptions-create (* rocksdb-readoptions))
(define-alien-routine rocksdb-readoptions-destroy void
  (opt (* rocksdb-readoptions)))

;;;; flush-opts
(define-alien-routine rocksdb-flush void 
  (db (* rocksdb))
  (options (* rocksdb-flushoptions))
  (errptr rocksdb-errptr))
;;; DB
(define-alien-routine rocksdb-open (* rocksdb)
  (opt (* rocksdb-options))
  (name c-string) 
  (errptr rocksdb-errptr))
(define-alien-routine rocksdb-close void 
  (db (* rocksdb)))
(define-alien-routine rocksdb-cancel-all-background-work void 
  (db (* rocksdb))
  (wait boolean))

(define-alien-routine rocksdb-put void 
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (key (* char))
  (keylen size-t) 
  (val (* char))
  (vallen size-t) 
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-get (* char)
  (db (* rocksdb))
  (options (* rocksdb-readoptions))
  (key (* char))
  (keylen size-t) 
  (vallen (* size-t))
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-delete void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (key (* char))
  (keylen size-t)
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-merge void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t)
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-merge-cf void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t)
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-write void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (batch (* rocksdb-writebatch))
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-get-cf (* char)
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (key (* char))
  (keylen size-t)
  (vallen (* size-t))
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-multi-get void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (num-keys size-t)
  (keys-list (array c-string))
  (keys-list-sizes (array size-t))
  (values-list (array c-string))
  (values-list-sizes (array size-t))
  (errs rocksdb-errptr))

(define-alien-routine rocksdb-multi-get-cf void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (cfs (array rocksdb-column-family-handle))
  (num-keys size-t)
  (keys-list (array c-string))
  (keys-list-sizes (array size-t))
  (values-list (array c-string))
  (values-list-sizes (array size-t))
  (errs rocksdb-errptr))

;;; CF
(define-alien-routine rocksdb-create-column-family (* rocksdb-column-family-handle)
  (db (* rocksdb))
  (column-family-options (* rocksdb-options))
  (column-family-name c-string)
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-create-column-families (array rocksdb-column-family-handle)
  (db (* rocksdb))
  (column-family-options (* rocksdb-options))
  (num-column-familes int)
  (column-family-names (array c-string))
  (lencfs (* size-t))
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-create-column-families-destroy void
  (list (array rocksdb-column-family-handle)))

(define-alien-routine rocksdb-column-family-handle-destroy void
  (* rocksdb-column-family-handle))

(define-alien-routine rocksdb-column-family-handle-get-id unsigned-int
  (* rocksdb-column-family-handle))

(define-alien-routine rocksdb-column-family-handle-get-name c-string
  (handle (* rocksdb-column-family-handle))
  (name-len (* size-t)))

(define-alien-routine rocksdb-drop-column-family void
  (db (* rocksdb))
  (handle (* rocksdb-column-family-handle))
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-open-column-families (* rocksdb)
  (options (* rocksdb-options))
  (name c-string)
  (num-column-families int)
  (column-family-names (array c-string))
  (column-family-options (array rocksdb-options))
  (column-family-handles (array rocksdb-column-family-handle))
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-list-column-families (array c-string)
  (opt (* rocksdb-options))
  (name c-string)
  (lencf (* size-t))
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-list-column-families-destroy void
  (list (array c-string))
  (len size-t))

(define-alien-routine rocksdb-put-cf void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t)
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-delete-cf void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* char))
  (keylen size-t)
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-delete-range-cf void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (start-key (* char))
  (start-key-len size-t)
  (end-key (* char))
  (end-key-len size-t)
  (errptr rocksdb-errptr))

;;; Iterators
(define-alien-routine rocksdb-create-iterator (* rocksdb-iterator)
  (db (* rocksdb))
  (opt (* rocksdb-readoptions)))
(define-alien-routine rocksdb-iter-destroy void 
  (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-seek-to-first void 
  (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-valid boolean 
  (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-next void 
  (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-prev void 
  (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-key (* char)
  (iter (* rocksdb-iterator))
  (klen-ptr (* size-t)))
(define-alien-routine rocksdb-iter-value (* char) 
  (iter (* rocksdb-iterator)) (vlen-ptr (* size-t)))
(define-alien-routine rocksdb-destroy-db void
  (options (* rocksdb-options))
  (name c-string) 
  (errptr rocksdb-errptr))
