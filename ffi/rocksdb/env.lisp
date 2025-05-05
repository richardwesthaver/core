;;; env.lisp --- RocksDB Env

;; 

;;; Code:
(in-package :rocksdb)

(define-alien-routine rocksdb-create-default-env (* rocksdb-env))

(define-alien-routine rocksdb-create-mem-env (* rocksdb-env))

(define-alien-routine rocksdb-env-set-background-threads void 
  (env (* rocksdb-env))
  (n int))

(define-alien-routine rocksdb-env-set-high-priority-background-threads void
  (env (* rocksdb-env)) (n int))

(define-alien-routine rocksdb-env-set-low-priority-background-threads void
  (env (* rocksdb-env)) (n int))

(define-alien-routine rocksdb-env-get-high-priority-background-threads int
  (env (* rocksdb-env)))

(define-alien-routine rocksdb-env-get-low-priority-background-threads int
  (env (* rocksdb-env)))

(define-alien-routine rocksdb-env-set-bottom-priority-background-threads void
  (env (* rocksdb-env)) (n int))

(define-alien-routine rocksdb-env-get-bottom-priority-background-threads int
  (env (* rocksdb-env)))

(define-alien-routine rocksdb-env-join-all-threads void
  (env (* rocksdb-env)))

(define-alien-routine rocksdb-env-lower-thread-pool-io-priority void
  (env (* rocksdb-env)))

(define-alien-routine rocksdb-env-lower-thread-pool-cpu-priority void (env (* rocksdb-env)))

(define-alien-routine rocksdb-env-lower-high-priority-thread-pool-cpu-priority void (env (* rocksdb-env)))
(define-alien-routine rocksdb-env-get-background-threads int
  (env (* rocksdb-env)))

(define-alien-routine rocksdb-env-destroy void (opt (* rocksdb-env)))

(def-with-errptr rocksdb-create-dir-if-missing void
  (env (* rocksdb-env))
  (path c-string))
