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

(def-with-errptr rocksdb-open-and-trim-history (* rocksdb)
  (opts (* rocksdb-options))
  (name c-string)
  (num-cfs int)
  (cf-names (array c-string))
  (cf-handles (array (* rocksdb-column-family-handle)))
  (trim-ts c-string)
  (trim-tslen size-t))

(def-with-errptr rocksdb-open-column-families 
  (* rocksdb)
  (options (* rocksdb-options))
  (name c-string)
  (num-column-families int)
  (column-family-names (array c-string))
  (column-family-options (array rocksdb-options))
  (column-family-handles (array rocksdb-column-family-handle)))

(def-with-errptr rocksdb-open-column-families-with-ttl (* rocksdb)
  (opts (* rocksdb-options))
  (name c-string)
  (num-cfs int)
  (cf-names (array c-string))
  (cf-opts (array (* rocksdb-options)))
  (cf-handles (array (* rocksdb-column-family-handle)))
  (ttls (array int)))

(def-with-errptr rocksdb-open-for-read-only-column-families (* rocksdb)
  (opts (* rocksdb-options))
  (name c-string)
  (num-cfs int)
  (cf-names (array c-string))
  (cf-opts (array (* rocksdb-options)))
  (cf-handles (array (* rocksdb-column-family-handle)))
  (err-if-wal-exists unsigned-char))
  
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
