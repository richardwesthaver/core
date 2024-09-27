;;; env.lisp --- RocksDB Env

;; 

;;; Code:
(in-package :rocksdb)

(define-alien-routine rocksdb-create-default-env (* rocksdb-env))
(define-alien-routine rocksdb-create-mem-env (* rocksdb-env))
(define-alien-routine rocksdb-env-set-background-threads void 
  (env (* rocksdb-env))
  (n int))
(define-alien-routine rocksdb-env-get-background-threads int
  (env (* rocksdb-env)))
(define-alien-routine rocksdb-env-destroy void (opt (* rocksdb-env)))
(def-with-errptr rocksdb-create-dir-if-missing void
  (env (* rocksdb-env))
  (path c-string))
