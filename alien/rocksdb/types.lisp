;;; rocksdb/types.lisp --- Rocksdb FFI Types

;;

;;; Code:
(in-package :rocksdb)

(define-alien-type rocksdb-errptr (* (* t)))
(define-opaque rocksdb)
(define-opaque rocksdb-iterator)
(define-opaque rocksdb-backup-engine)
(define-opaque rocksdb-backup-engine-info)
(define-opaque rocksdb-memory-allocator)
(define-opaque rocksdb-cache)
(define-opaque rocksdb-compactionfilter)
(define-opaque rocksdb-compactionfiltercontext)
(define-opaque rocksdb-compactionfilterfactory)
(define-opaque rocksdb-file-checksum-gen-factory)
(define-opaque rocksdb-sst-partitioner-factory)
(define-opaque rocksdb-table-properties-collector-factory)
(define-opaque rocksdb-comparator)
(define-opaque rocksdb-dbpath)
(define-opaque rocksdb-env)
(define-opaque rocksdb-filelock)
(define-opaque rocksdb-filterpolicy)
(define-opaque rocksdb-logger)
(define-opaque rocksdb-mergeoperator)
(define-opaque rocksdb-randomfile)
(define-opaque rocksdb-seqfile)
(define-opaque rocksdb-slicetransform)
(define-opaque rocksdb-snapshot)
(define-opaque rocksdb-writeablefile)
(define-opaque rocksdb-writebatch)
(define-opaque rocksdb-writebatch-wi)
(define-opaque rocksdb-livefiles)
(define-opaque rocksdb-column-family-handle)
(define-opaque rocksdb-column-family-metadata)
(define-opaque rocksdb-level-metadata)
(define-opaque rocksdb-sst-file-manager)
(define-opaque rocksdb-sst-file-metadata)
(define-opaque rocksdb-sstfilewriter)
(define-opaque rocksdb-ratelimiter)
(define-opaque rocksdb-perfcontext)
(define-opaque rocksdb-pinnableslice)
(define-opaque rocksdb-pinnable-handle)
(define-opaque rocksdb-transactiondb)
(define-opaque rocksdb-optimistictransactiondb)
(define-opaque rocksdb-transaction)
(define-opaque rocksdb-checkpoint)
(define-opaque rocksdb-wal-iterator)
(define-opaque rocksdb-memory-consumers)
(define-opaque rocksdb-memory-usage)
(define-opaque rocksdb-statistics-histogram-data)
(define-opaque rocksdb-status-ptr)
(define-opaque rocksdb-flushjobinfo)
(define-opaque rocksdb-writestallcondition)
(define-opaque rocksdb-writestallinfo)
(define-opaque rocksdb-memtableinfo)
(define-opaque rocksdb-compactionservice)
(define-opaque rocksdb-compactionservice-scheduleresponse)
(define-opaque rocksdb-compactionservice-jobinfo)
(define-opaque rocksdb-compactionjobinfo)
(define-opaque rocksdb-subcompactionjobinfo)
(define-opaque rocksdb-externalfileingestioninfo)
(define-opaque rocksdb-eventlistener)
(define-opaque rocksdb-pinnable-multi-get)
(define-opaque rocksdb-export-import-files-metadata)
(define-opaque rocksdb-table-properties)
(define-alien-enum (rocksdb-size-approximation-flags)
  :none 0
  :include-memtable (ash 1 0)
  :include-files (ash 1 1)
  :include-blob-files (ash 1 2))
(define-alien-enum (rocksdb-compactionservice-jobstatus)
  :success 0
  :failure 1
  :aborted 2
  :use-local 3)

(define-alien-type rocksdb-slice
  (struct rocksdb-slice
    (data (* unsigned-char))
    (size size-t)))

(define-alien-type rocksdb-readoptions-table-filter-cb
    (function unsigned-char
        (* t)
        (* rocksdb-table-properties)))

(eval-always
  (defvar *rocksdb-partial-merge-lambda-list*
    '((state (* t))
      (key (* unsigned-char))
      (klen size-t)
      (ops (* (* unsigned-char)))
      (ops-length (* size-t))
      (num-ops size-t)
      (success (* unsigned-char))
      (new-vlen (* size-t))))

  (defvar *rocksdb-full-merge-lambda-list*
    '((state (* t))
      (key (* unsigned-char))
      (klen size-t)
      (existing-val (* unsigned-char))
      (existing-vlen size-t)
      (ops (* (* unsigned-char)))
      (ops-length (* size-t))
      (num-ops int)
      (success (* unsigned-char))
      (new-vlen (* size-t)))))

#|
Gives the client a way to express the read -> modify -> write semantics
key:         (IN) The key that's associated with this merge operation.
existing:    (IN) null indicates that the key does not exist before this op
operand_list:(IN) the sequence of merge operations to apply, front() first.
new_value:  (OUT) Client is responsible for filling the merge result here
logger:      (IN) Client could use this to log errors during merge.

Return true on success. Return false failure / error / corruption.
|#
;; FullMerge() is used when a Put/Delete is the *existing_value (or null)
(define-alien-type rocksdb-full-merge-function
    (function (* char)
        (* t)
        (* unsigned-char)
      size-t
      (* unsigned-char)
      size-t
      (* (* unsigned-char))
      (* size-t)
      int
      (* unsigned-char)
      (* size-t)))

#|
This function performs merge(left_op, right_op)
when both the operands are themselves merge operation types.
Save the result in *new_value and return true. If it is impossible
or infeasible to combine the two operations, return false instead.
|#
;; PartialMerge() is used to combine two-merge operands (if possible)
(define-alien-type rocksdb-partial-merge-function
    (function (* char)
        (* t)
        (* unsigned-char)
      size-t
      (* (* unsigned-char))
      (* size-t)
      int
      (* unsigned-char)
      (* size-t)))

(define-alien-type rocksdb-delete-value-function
    (function void
        (* unsigned-char)
        size-t))

(define-alien-type rocksdb-destructor-function
    (function void (* t)))

#|
The name of the MergeOperator. Used to check for MergeOperator
mismatches (i.e., a DB created with one MergeOperator is
accessed using a different MergeOperator)
|#
(define-alien-type rocksdb-name-function
    (function c-string))

