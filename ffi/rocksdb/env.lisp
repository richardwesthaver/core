;;; env.lisp --- RocksDB Env

;; 

;;; Code:
(in-package :rocksdb)

(defar rocksdb-create-default-env (* rocksdb-env))

(defar rocksdb-create-mem-env (* rocksdb-env))

(defar rocksdb-env-set-background-threads void 
  (env (* rocksdb-env))
  (n int))

(defar rocksdb-env-set-high-priority-background-threads void
  (env (* rocksdb-env)) (n int))

(defar rocksdb-env-set-low-priority-background-threads void
  (env (* rocksdb-env)) (n int))

(defar rocksdb-env-get-high-priority-background-threads int
  (env (* rocksdb-env)))

(defar rocksdb-env-get-low-priority-background-threads int
  (env (* rocksdb-env)))

(defar rocksdb-env-set-bottom-priority-background-threads void
  (env (* rocksdb-env)) (n int))

(defar rocksdb-env-get-bottom-priority-background-threads int
  (env (* rocksdb-env)))

(defar rocksdb-env-join-all-threads void
  (env (* rocksdb-env)))

(defar rocksdb-env-lower-thread-pool-io-priority void
  (env (* rocksdb-env)))

(defar rocksdb-env-lower-thread-pool-cpu-priority void (env (* rocksdb-env)))

(defar rocksdb-env-lower-high-priority-thread-pool-cpu-priority void (env (* rocksdb-env)))
(defar rocksdb-env-get-background-threads int
  (env (* rocksdb-env)))

(defar rocksdb-env-destroy void (opt (* rocksdb-env)))

(def-with-errptr rocksdb-create-dir-if-missing void
  (env (* rocksdb-env))
  (path c-string))
