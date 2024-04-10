;;; rocksdb/compaction.lisp --- RocksDB Compaction

;;

;;; Code:
(in-package :rocksdb)
;; (define-alien-routine rocksdb-compactionfilter-create (* rocksdb-compactionfilter)
;;   (state (* void))
;;   (destructor (* void))
;;   (filter (* unsigned-char))
;;   (name (* unsigned-char)))

(define-alien-routine rocksdb-compactionfilter-set-ignore-snapshots void
  (self (* rocksdb-compactionfilter)) (val unsigned-char))

(define-alien-routine rocksdb-compactionfilter-destroy void
  (self (* rocksdb-compactionfilter)))

;;; Compaction Filter Context
(define-alien-routine rocksdb-compactionfiltercontext-is-full-compaction unsigned-char
  (context (* rocksdb-compactionfiltercontext)))

(define-alien-routine rocksdb-compactionfiltercontext-is-manual-compaction unsigned-char
  (context (* rocksdb-compactionfiltercontext)))

(export '(rocksdb-compactionfilter-set-ignore-snapshots rocksdb-compactionfilter-destroy
          rocksdb-compactionfiltercontext-is-full-compaction rocksdb-compactionfiltercontext-is-manual-compaction))

;;; Compaction Filter Factory
