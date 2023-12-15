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
(define-opaque rocksdb-livefiles)
(define-opaque rocksdb-column-family-handle)
(define-opaque rocksdb-column-family-metadata)
(define-opaque rocksdb-level-metadata)
(define-opaque rocksdb-sst-file-metadata)
(define-opaque rocksdb-sstfilewriter)
(define-opaque rocksdb-ratelimiter)
(define-opaque rocksdb-perfcontext)
(define-opaque rocksdb-pinnableslice)
(define-opaque rocksdb-transactiondb)
(define-opaque rocksdb-optimistictransactiondb)
(define-opaque rocksdb-transaction)
(define-opaque rocksdb-checkpoint)
(define-opaque rocksdb-wal-iterator)
(define-opaque rocksdb-memory-comsumers)
(define-opaque rocksdb-memory-usage)
(define-opaque rocksdb-statistics-histogram-data)
