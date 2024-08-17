;;; rocksdb/compaction.lisp --- RocksDB Compaction

;; RocksDB Lisp Compaction Filter API

;;; Commentary:

;; compaction filters are like custom GC rules for the database. compactions
;; run in the background and can be configured via the column-family-options
;; or compactionfilterfactory API.

;; ref: https://github.com/facebook/rocksdb/wiki/Compaction-Filter

;;; Code:
(in-package :rocksdb)

(define-alien-type rocksdb-filter-function
  (function unsigned-char
            (* t)
            int
            c-string
            size-t
            c-string
            size-t
            (* (array unsigned-char))
            (* size-t)
            (* unsigned-char)))

(define-alien-type rocksdb-create-compaction-filter-function
    (function (* rocksdb-compactionfilter)
              (* t)
              (* rocksdb-compactionfiltercontext)))
            
(define-alien-routine rocksdb-compactionfilter-set-ignore-snapshots void
  (self (* rocksdb-compactionfilter)) (val unsigned-char))

(define-alien-routine rocksdb-compactionfilter-destroy void
  (self (* rocksdb-compactionfilter)))

;;; Compaction Filter Context
(define-alien-routine rocksdb-compactionfiltercontext-is-full-compaction unsigned-char
  (context (* rocksdb-compactionfiltercontext)))

(define-alien-routine rocksdb-compactionfiltercontext-is-manual-compaction unsigned-char
  (context (* rocksdb-compactionfiltercontext)))

;;; Compaction Filter Factory
(define-alien-routine rocksdb-compactionfilter-create (* rocksdb-compactionfilter)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (generator (* rocksdb-create-compaction-filter-function))
  (context (* rocksdb-compactionfiltercontext)))

(define-alien-routine rocksdb-compacitonfilter-destroy void
  (factory (* rocksdb-compactionfilterfactory)))

;; maybe not possible? test
(define-alien-callable rocksdb-create-compaction-filter (* rocksdb-compactionfilter)
    ((state (* t))
     (context (* rocksdb-compactionfiltercontext)))
  (declare (ignore state context)))

