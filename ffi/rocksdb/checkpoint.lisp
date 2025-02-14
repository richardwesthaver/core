;;; rocksdb/checkpoint.lisp --- RocksDB Checkpoints

;;

;;; Code:
(in-package :rocksdb)

(def-with-errptr rocksdb-checkpoint-object-create (* rocksdb-checkpoint)
  (db (* rocksdb)))

(def-with-errptr rocksdb-checkpoint-create void
  (checkpoint (* rocksdb-checkpoint))
  (checkpoint-dir c-string)
  (log-size-for-flush (unsigned 64)))

(define-alien-routine rocksdb-checkpoint-object-destroy void
  (checkpoint (* rocksdb-checkpoint)))
