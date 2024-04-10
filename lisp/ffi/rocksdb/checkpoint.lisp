;;; rocksdb/checkpoint.lisp --- RocksDB Checkpoints

;;

;;; Code:
(in-package :rocksdb)

(export '(rocksdb-checkpoint-object-destroy))

(def-with-errptr rocksdb-checkpoint-object-create (* rocksdb-checkpoint)
  (db (* rocksdb)))

(def-with-errptr rocksdb-checkpoint-create void
  (checkpoint (* rocksdb-checkpoint))
  (checkpoint-dir c-string)
  (log-size-for-flush (unsigned 64)))

(define-alien-routine rocksdb-checkpoint-object-destroy void
  (* rocksdb-checkpoint))

;; rocksdb-open-and-trim-history
(def-with-errptr rocksdb-open-as-secondary-column-families (* rocksdb)
  (opts (* rocksdb-options))
  (name c-string)
  (secondary-path c-string)
  (num-cfs int)
  (cf-names (array c-string))
  (cf-opts (array (* rocksdb-options)))
  (cf-handles (array (* rocksdb-column-family-handle))))

(def-with-errptr rocksdb-open-as-secondary (* rocksdb)
  (opts (* rocksdb-options))
  (name c-string)
  (secondary-path c-string))
