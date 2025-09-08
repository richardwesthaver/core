;;; db.lisp --- RocksDB Alien Database Functions

;; 

;;; Code:
(in-package :rocksdb)

;;; DB
(def-with-errptr rocksdb-open (* rocksdb)
  (opt (* rocksdb-options))
  (name c-string))

(def-with-errptr rocksdb-open-and-trim-history (* rocksdb)
  (opts (* rocksdb-options))
  (name c-string)
  (num-cfs int)
  (cf-names (* c-string))
  (cf-handles (* (* rocksdb-column-family-handle)))
  (trim-ts c-string)
  (trim-tslen size-t))

(def-with-errptr rocksdb-open-column-families (* rocksdb)
  (options (* rocksdb-options))
  (name c-string)
  (num-column-families int)
  (column-family-names (* (* char)))
  (column-family-options (* (* rocksdb-options)))
  (column-family-handles (* (* rocksdb-column-family-handle))))

(def-with-errptr rocksdb-open-column-families-with-ttl (* rocksdb)
  (opts (* rocksdb-options))
  (name c-string)
  (num-cfs int)
  (cf-names (* c-string))
  (cf-opts (* (* rocksdb-options)))
  (cf-handles (* (* rocksdb-column-family-handle)))
  (ttls (* int)))

(def-with-errptr rocksdb-open-for-read-only-column-families (* rocksdb)
  (opts (* rocksdb-options))
  (name c-string)
  (num-cfs int)
  (cf-names (* c-string))
  (cf-opts (* (* rocksdb-options)))
  (cf-handles (* (* rocksdb-column-family-handle)))
  (err-if-wal-exists unsigned-char))

(def-with-errptr rocksdb-open-as-secondary-column-families (* rocksdb)
  (opts (* rocksdb-options))
  (name c-string)
  (secondary-path c-string)
  (num-cfs int)
  (cf-names (* c-string))
  (cf-opts (* (* rocksdb-options)))
  (cf-handles (* (* rocksdb-column-family-handle))))

(def-with-errptr rocksdb-open-as-secondary (* rocksdb)
  (opts (* rocksdb-options))
  (name c-string)
  (secondary-path c-string))

(defar rocksdb-close void 
  (db (* rocksdb)))

(defar rocksdb-cancel-all-background-work void 
  (db (* rocksdb))
  (wait boolean))

(defar rocksdb-disable-manual-compaction void
  (db (* rocksdb)))

(defar rocksdb-enable-manual-compaction void
  (db (* rocksdb)))

(def-with-errptr rocksdb-put 
  void 
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (key (* unsigned-char))
  (keylen size-t) 
  (val (* unsigned-char))
  (vallen size-t))

(def-with-errptr rocksdb-write void
  (db (* rocksdb))
  (opts (* rocksdb-writeoptions))
  (batch (* rocksdb-writebatch)))

(def-with-errptr rocksdb-get 
    (* unsigned-char)
  (db (* rocksdb))
  (options (* rocksdb-readoptions))
  (key (* unsigned-char))
  (keylen size-t) 
  (vallen (* size-t)))

(def-with-errptr rocksdb-get-with-ts c-string
  (db (* rocksdb))
  (opts (* rocksdb-readoptions))
  (key c-string)
  (keylen size-t)
  (vallen (* size-t))
  (ts (* c-string))
  (tslen (* size-t)))

(def-with-errptr rocksdb-delete 
  void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (key (* unsigned-char))
  (keylen size-t))

(def-with-errptr rocksdb-merge 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (key (* unsigned-char))
  (keylen size-t)
  (val (* unsigned-char))
  (vallen size-t))

(def-with-errptr rocksdb-merge-cf 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (keylen size-t)
  (val (* unsigned-char))
  (vallen size-t))

(def-with-errptr rocksdb-get-cf 
    (* unsigned-char)
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (keylen size-t)
  (vallen (* size-t)))

(def-with-errptr rocksdb-get-cf-with-ts c-string
  (db (* rocksdb))
  (opts (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle))  
  (key c-string)
  (keylen size-t)
  (vallen (* size-t))
  (ts (* c-string))
  (tslen (* size-t)))

(defar rocksdb-get-db-identity c-string (db (* rocksdb)) (idlen (* size-t)))

;; NOTE 2023-12-19: only the VOID-returning functions in the multi-
;; family perform parallel IO:
;; https://github.com/facebook/rocksdb/wiki/MultiGet-Performance
(defar rocksdb-multi-get void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (num-keys size-t)
  (keys-list (* c-string))
  (keys-list-sizes (* size-t))
  (values-list (* c-string))
  (values-list-sizes (* size-t))
  (errs (* rocksdb-errptr)))

(defar rocksdb-multi-get-with-ts void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (num-keys size-t)
  (keys-list (* c-string))
  (keys-list-sizes (* size-t))
  (values-list (* c-string))
  (values-list-sizes (* size-t))
  (ts-list (* c-string))
  (ts-list-sizes (* size-t))
  (errs (* rocksdb-errptr)))

(defar rocksdb-multi-get-cf void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (cfs (* (* rocksdb-column-family-handle)))
  (num-keys size-t)
  (keys-list (* (* (unsigned 8))))
  (keys-list-sizes (* size-t))
  (values-list (* (* (unsigned 8))))
  (values-list-sizes (* size-t))
  (errs (* rocksdb-errptr)))

(defar rocksdb-multi-get-cf-with-ts void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (cfs (* rocksdb-column-family-handle))
  (num-keys size-t)
  (keys-list (* c-string))
  (keys-list-sizes (* size-t))
  (values-list (* c-string))
  (values-list-sizes (* size-t))
  (ts-list (* c-string))
  (ts-list-sizes (* size-t))
  (errs (* rocksdb-errptr)))

(defar rocksdb-batched-multi-get-cf void
  (db (* rocksdb))
  (opts (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle))
  (nkeys size-t)
  (keys (* c-string))
  (key-sizes (* size-t))
  (values (* (* rocksdb-pinnableslice)))
  (errs (* (* rocksdb-errptr)))
  (sorted-input boolean))

(defar rocksdb-key-may-exist unsigned-char
  (db (* rocksdb))
  (opts (* rocksdb-readoptions))
  (key c-string)
  (key-len size-t)
  (value (* c-string))
  (val-len (* size-t))
  (timestamp c-string)
  (timestamp-len size-t)
  (value-found (* unsigned-char)))

(defar rocksdb-key-may-exist-cf unsigned-char
  (db (* rocksdb))
  (opts (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle))
  (key c-string)
  (key-len size-t)
  (value (* c-string))
  (val-len (* size-t))
  (timestamp c-string)
  (timestamp-len size-t)
  (value-found (* unsigned-char)))

(defar rocksdb-cache-create-lru (* rocksdb-cache) (capacity size-t))

(defar rocksdb-cache-create-hyper-clock (* rocksdb-cache)
  (capacity size-t) (estimated-entry-charge size-t))

(defar rocksdb-cache-create-hyper-clock-opts (* rocksdb-cache)
  (opts (* rocksdb-hyper-clock-cache-options)))

(def-with-errptr rocksdb-flush void 
  (db (* rocksdb))
  (options (* rocksdb-flushoptions)))

(def-with-errptr rocksdb-flush-cf void
  (db (* rocksdb))
  (opts (* rocksdb-flushoptions))
  (cf (* rocksdb-column-family-handle))
  (num-cf int))

(def-with-errptr rocksdb-flush-cfs void
  (db (* rocksdb))
  (opts (* rocksdb-flushoptions))
  (cf (* (* rocksdb-column-family-handle)))
  (num-cf int))

(def-with-errptr rocksdb-flush-wal void
  (db (* rocksdb))
  (sync unsigned-char))

;; deprecated AO 2025-01-12
;; (defar rocksdb-delete-file void
;;   (db (* rocksdb))
;;   (name c-string))

(defar rocksdb-livefiles-count int (lf (* rocksdb-livefiles)))

(defar rocksdb-livefiles (* rocksdb-livefiles)
  (db (* rocksdb)))

(defar rocksdb-livefile (* rocksdb-livefiles)
  (db (* rocksdb))
  (name c-string))

(defar rocksdb-livefiles-column-family-name c-string (lf (* rocksdb-livefiles)) (index int))
(defar rocksdb-livefiles-name c-string (lf (* rocksdb-livefiles)) (index int))
(defar rocksdb-livefiles-level int (lf (* rocksdb-livefiles)) (index int))
(defar rocksdb-livefiles-size size-t (lf (* rocksdb-livefiles)) (index int))
(defar rocksdb-livefiles-smallest-key c-string (lf (* rocksdb-livefiles)) (index int) (size (* size-t)))
(defar rocksdb-livefiles-largest-key c-string (lf (* rocksdb-livefiles)) (index int) (size (* size-t)))
(defar rocksdb-livefiles-entries unsigned-long (lf (* rocksdb-livefiles)) (index int))
(defar rocksdb-livefiles-deletions unsigned-long (lf (* rocksdb-livefiles)) (index int))
;; return NULL if prop name is unknown, else return pointer to
;; malloc-ed null-term value.
(defar rocksdb-property-value c-string
  (db (* rocksdb))
  (propname c-string))

;; return 0 on success, else -1
(defar rocksdb-property-int int
  (db (* rocksdb))
  (propname c-string))

(defar rocksdb-property-value-cf c-string
  (db (* rocksdb))
  (cf (* rocksdb-column-family-handle))
  (propname c-string))

(defar rocksdb-property-int-cf int
  (db (* rocksdb))
  (cf (* rocksdb-column-family-handle))
  (propname c-string))

;;; CF
(def-with-errptr rocksdb-create-column-family 
    (* rocksdb-column-family-handle)
  (db (* rocksdb))
  (column-family-options (* rocksdb-options))
  (column-family-name c-string))

(def-with-errptr rocksdb-create-column-families
    (* rocksdb-column-family-handle)
  (db (* rocksdb))
  (column-family-options (* rocksdb-options))
  (num-column-familes int)
  (column-family-names (* c-string))
  (lencfs (* size-t)))

(defar rocksdb-create-column-families-destroy void
  (list (* rocksdb-column-family-handle)))

(def-with-errptr rocksdb-create-column-family-with-ttl (* rocksdb-column-family-handle)
  (db (* rocksdb))
  (cf-opts (* rocksdb-options))
  (cf-name c-string)
  (ttl int))

(defar rocksdb-column-family-handle-destroy void
  (cf (* rocksdb-column-family-handle)))

(defar rocksdb-column-family-handle-get-id unsigned-int
  (cf (* rocksdb-column-family-handle)))

(defar rocksdb-column-family-handle-get-name c-string
  (handle (* rocksdb-column-family-handle))
  (name-len (* size-t)))

(def-with-errptr rocksdb-drop-column-family 
  void
  (db (* rocksdb))
  (handle (* rocksdb-column-family-handle)))

(defar rocksdb-get-default-column-family-handle (* rocksdb-column-family-handle)
  (db (* rocksdb)))

(def-with-errptr rocksdb-list-column-families 
    (* c-string)
  (opt (* rocksdb-options))
  (name c-string)
  (lencf (* size-t)))

(defar rocksdb-list-column-families-destroy void
  (list (* c-string))
  (len size-t))

(def-with-errptr rocksdb-put-cf 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (keylen size-t)
  (val (* unsigned-char))
  (vallen size-t))

(def-with-errptr rocksdb-put-with-ts void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (key (* unsigned-char))
  (keylen size-t)
  (ts c-string)
  (tslen size-t)
  (val (* unsigned-char))
  (vallen size-t))

(def-with-errptr rocksdb-put-cf-with-ts void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (keylen size-t)
  (ts c-string)
  (tslen size-t)
  (val (* unsigned-char))
  (vallen size-t))

(def-with-errptr rocksdb-delete-cf 
  void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (keylen size-t))

(def-with-errptr rocksdb-delete-with-ts
  void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (key (* unsigned-char))
  (ts c-string)
  (tslen size-t)
  (keylen size-t))

(def-with-errptr rocksdb-delete-cf-with-ts
  void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (ts c-string)
  (tslen size-t)
  (keylen size-t))

(def-with-errptr rocksdb-singledelete void
  (db (* rocksdb))
  (opts (* rocksdb-writeoptions))
  (key c-string)
  (keylen size-t))

(def-with-errptr rocksdb-singledelete-with-ts void
  (db (* rocksdb))
  (opts (* rocksdb-writeoptions))
  (key c-string)
  (keylen size-t)
  (ts c-string)
  (tslen size-t))

(def-with-errptr rocksdb-singledelete-cf-with-ts void
  (db (* rocksdb))
  (opts (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key c-string)
  (keylen size-t)
  (ts c-string)
  (tslen size-t))

(def-with-errptr rocksdb-singledelete-cf void
  (db (* rocksdb))
  (opts (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key c-string)
  (keylen size-t))

(def-with-errptr rocksdb-increase-full-history-ts-low void
  (db (* rocksdb))
  (cf (* rocksdb-column-family-handle))
  (ts-low c-string)
  (ts-lowlen size-t))

(def-with-errptr rocksdb-get-full-history-ts-low c-string
  (db (* rocksdb))
  (cf (* rocksdb-column-family-handle))
  (ts-lowlen (* size-t)))

(def-with-errptr rocksdb-delete-range-cf 
  void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (start-key (* unsigned-char))
  (start-key-len size-t)
  (end-key (* unsigned-char))
  (end-key-len size-t))

(def-with-errptr rocksdb-disable-file-deletions void
  (db (* rocksdb)))

(def-with-errptr rocksdb-enable-file-deletions void
  (db (* rocksdb)))

(def-with-errptr rocksdb-destroy-db void
  (opts (* rocksdb-options))
  (name c-string))

(def-with-errptr rocksdb-repair-db void
  (opts (* rocksdb-options))
  (name c-string))

;;; Iterators
(defar rocksdb-create-iterator (* rocksdb-iterator)
  (db (* rocksdb))
  (opt (* rocksdb-readoptions)))

(defar rocksdb-get-updates-since (* rocksdb-wal-iterator)
  (db (* rocksdb))
  (opts (* rocksdb-readoptions)))

(defar rocksdb-create-iterator-cf (* rocksdb-iterator)
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle)))

(def-with-errptr rocksdb-create-iterators void
  (db (* rocksdb))
  (opts (* rocksdb-readoptions))
  (cfs (* (* rocksdb-column-family-handle)))
  (iters (* (* rocksdb-iterator))))

(defar rocksdb-iter-destroy void 
  (iter (* rocksdb-iterator)))
(defar rocksdb-iter-seek-to-first void 
  (iter (* rocksdb-iterator)))
(defar rocksdb-iter-seek-to-last void 
  (iter (* rocksdb-iterator)))
(defar rocksdb-iter-seek void 
  (iter (* rocksdb-iterator))
  (k (* unsigned-char))
  (klen size-t))
(defar rocksdb-iter-seek-for-prev void 
  (iter (* rocksdb-iterator))
  (k (* unsigned-char))
  (klen size-t))
(defar rocksdb-iter-valid boolean 
  (iter (* rocksdb-iterator)))
(defar rocksdb-iter-next void
  (iter (* rocksdb-iterator)))
(defar rocksdb-iter-prev void 
  (iter (* rocksdb-iterator)))
(defar rocksdb-iter-key (* unsigned-char)
  (iter (* rocksdb-iterator))
  (klen-ptr (* size-t)))
(defar rocksdb-iter-value (* unsigned-char) 
  (iter (* rocksdb-iterator)) 
  (vlen-ptr (* size-t)))
(defar rocksdb-iter-timestamp (* unsigned-char) 
  (iter (* rocksdb-iterator))
  (tslen (* size-t)))
(def-with-errptr rocksdb-iter-get-error void (iter (* rocksdb-iterator)))
(defar rocksdb-wal-iter-next void (iter (* rocksdb-wal-iterator)))
(defar rocksdb-wal-iter-valid unsigned-char (iter (* rocksdb-wal-iterator)))
(def-with-errptr rocksdb-wal-iter-status unsigned-char (iter (* rocksdb-wal-iterator)))
(defar rocksdb-wal-iter-get-batch (* rocksdb-writebatch)
  (iter (* rocksdb-wal-iterator))
  (seq (* (unsigned 64))))
(defar rockdsb-get-latest-sequence-number (unsigned 64) (db (* rocksdb)))
(defar rocksdb-wal-iter-destroy void
  (iter (* rocksdb-wal-iterator)))

;;; Backup
(def-with-errptr rocksdb-backup-engine-verify-backup void
  (be (* rocksdb-backup-engine))
  (backup-id (unsigned 32)))

(def-with-errptr rocksdb-backup-engine-open
    (* rocksdb-backup-engine)
  (opts (* rocksdb-options))
  (path c-string))

(def-with-errptr rocksdb-backup-engine-create-new-backup
  void
  (be (* rocksdb-backup-engine))
  (db (* rocksdb)))

(def-with-errptr rocksdb-backup-engine-restore-db-from-latest-backup
  void
  (be (* rocksdb-backup-engine))
  (db-dir c-string)
  (wal-dir c-string)
  (res-opts (* rocksdb-restore-options)))

(def-with-errptr rocksdb-backup-engine-restore-db-from-backup
  void
  (be (* rocksdb-backup-engine))
  (db-dir c-string)
  (wal-dir c-string)
  (res-opts (* rocksdb-restore-options))
  (backup-id unsigned-int))

(defar rocksdb-backup-engine-close void
  (be (* rocksdb-backup-engine)))

(defar rocksdb-backup-engine-get-backup-info (* rocksdb-backup-engine-info)
  (be (* rocksdb-backup-engine)))
(defar rocksdb-backup-engine-info-count int
  (info (* rocksdb-backup-engine-info)))
(defar rocksdb-backup-engine-info-timestamp (signed 64)
  (info (* rocksdb-backup-engine-info))
  (index int))
(defar rocksdb-backup-engine-info-backup-id (unsigned 64)
  (info (* rocksdb-backup-engine-info))
  (index int))
(defar rocksdb-backup-engine-info-size (unsigned 64)
  (info (* rocksdb-backup-engine-info))
  (index int))
(defar rocksdb-backup-engine-info-num-files (unsigned 32)
  (info (* rocksdb-backup-engine-info))
  (index int))
(defar rocksdb-backup-engine-info-destroy void
  (info (* rocksdb-backup-engine-info)))

;;; Transactions
(def-with-errptr rocksdb-transactiondb-create-column-family (* rocksdb-column-family-handle)
  (txn-db (* rocksdb-transactiondb))
  (cf-options (* rocksdb-options))
  (cf-name c-string))

(def-with-errptr rocksdb-transactiondb-open (* rocksdb-transactiondb)
  (opts (* rocksdb-options))
  (topts (* rocksdb-transactiondb-options))
  (name c-string))

(def-with-errptr rocksdb-transactiondb-open-column-families (* rocksdb-transactiondb)
  (opts (* rocksdb-options))
  (txn-db-opts (* rocksdb-transactiondb-options))
  (name c-string)
  (num-cfs int)
  (cf-names (* c-string))
  (cf-opfs (* rocksdb-options))
  (cf-handles (* (* rocksdb-column-family-handle))))

(defar rocksdb-transactiondb-create-snapshot (* rocksdb-snapshot)
  (txn-db (* rocksdb-transactiondb))
  (snapshot (* rocksdb-snapshot)))

(defar rocksdb-transactiondb-release-snapshot void
  (txn-db (* rocksdb-transactiondb))
  (snapshot (* rocksdb-snapshot)))

(defar rocksdb-transactiondb-property-value c-string
  (db (* rocksdb-transactiondb))
  (propname c-string))

(defar rocksdb-transactiondb-property-int int
  (db (* rocksdb-transactiondb))
  (propname c-string)
  (out-val (unsigned 64)))

(defar rocksdb-transactiondb-get-base-db (* rocksdb)
  (txn-db (* rocksdb-transactiondb)))

(defar rocksdb-transactiondb-close-base-db void
  (base-db (* rocksdb)))

(defar rocksdb-transaction-begin (* rocksdb-transaction)
  (txn-db (* rocksdb-transactiondb))
  (wopts (* rocksdb-writeoptions))
  (topts (* rocksdb-transaction-options))
  (told (* rocksdb-transaction)))

(defar rocksdb-transactiondb-get-prepared-transactions (* (* rocksdb-transaction))
  (txn-db (* rocksdb-transactiondb))
  (cnt (* size-t)))

(def-with-errptr rocksdb-transaction-set-name void
  (txn (* rocksdb-transaction))
  (name (* unsigned-char))
  (name-len size-t))

(defar rocksdb-transaction-get-name (* unsigned-char)
  (txn (* rocksdb-transaction))
  (name-len (* size-t)))

(def-with-errptr rocksdb-transaction-prepare void
  (txn (* rocksdb-transaction)))

(def-with-errptr rocksdb-transaction-commit void
  (txn (* rocksdb-transaction)))

(def-with-errptr rocksdb-transaction-rollback void
  (txn (* rocksdb-transaction)))

(defar rocksdb-transaction-set-savepoint void
  (txn (* rocksdb-transaction)))

(def-with-errptr rocksdb-transaction-rollback-to-savepoint void
  (txn (* rocksdb-transaction)))

(defar rocksdb-transaction-destroy void
  (txn (* rocksdb-transaction)))

(defar rocksdb-transaction-get-writebach-wi (* rocksdb-writebatch-wi)
  (txn (* rocksdb-transaction)))

(def-with-errptr rocksdb-transaction-delete void
  (txn (* rocksdb-transaction))
  (key c-string)
  (klen size-t))

(def-with-errptr rocksdb-transaction-delete-cf void
  (txn (* rocksdb-transaction))
  (cf (* rocksdb-column-family-handle))
  (key c-string)
  (klen size-t))

(def-with-errptr rocksdb-transactiondb-delete void
  (txndb (* rocksdb-transactiondb))
  (opts (* rocksdb-writeoptions))
  (key c-string)
  (klen size-t))

(def-with-errptr rocksdb-transactiondb-delete-cf void
  (txndb (* rocksdb-transactiondb))
  (opts (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key c-string)
  (klen size-t))

(defar rocksdb-transaction-create-iterator (* rocksdb-iterator)
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions)))

(defar rocksdb-transaction-create-iterator-cf (* rocksdb-iterator)
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle)))

(def-with-errptr rocksdb-transaction-get (* unsigned-char)
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (key (* unsigned-char))
  (klen size-t)
  (vlen (* size-t)))

(def-with-errptr rocksdb-transaction-get-pinned (* rocksdb-pinnableslice)
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (key (* unsigned-char))
  (klen size-t))

(def-with-errptr rocksdb-transaction-get-cf (* unsigned-char)
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (cf-handle (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (klen size-t)
  (vlen (* size-t)))

(def-with-errptr rocksdb-transaction-get-pinned-cf (* rocksdb-pinnableslice)
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (cf-handle (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (klen size-t))

(def-with-errptr rocksdb-transaction-get-for-update (* unsigned-char)
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (key (* unsigned-char))
  (klen size-t)
  (vlen (* size-t))
  (exclusive unsigned-char))

(def-with-errptr rocksdb-transaction-get-pinned-for-update (* rocksdb-pinnableslice)
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (key (* unsigned-char))
  (klen size-t)
  (exclusive unsigned-char))

(def-with-errptr rocksdb-transaction-get-for-update-cf (* unsigned-char)
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (cf-handle (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (klen size-t)
  (vlen (* size-t))
  (exclusive unsigned-char))

(def-with-errptr rocksdb-transaction-get-pinned-for-update-cf (* rocksdb-pinnableslice)
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (cf-handle (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (klen size-t)
  (exclusive unsigned-char))

(def-with-errptr rocksdb-transaction-multi-get void
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (nkeys size-t)
  (keys (* (* unsigned-char)))
  (key-sizes (* size-t))
  (vals (* (* unsigned-char)))
  (val-sizes (* size-t)))

(def-with-errptr rocksdb-transaction-multi-get-for-update void
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (nkeys size-t)
  (keys (* (* unsigned-char)))
  (key-sizes (* size-t))
  (vals (* (* unsigned-char)))
  (val-sizes (* size-t)))

(def-with-errptr rocksdb-transaction-multi-get-cf void
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (cfs (* (* rocksdb-column-family-handle)))
  (nkeys size-t)
  (keys (* (* unsigned-char)))
  (key-sizes (* size-t))
  (vals (* (* unsigned-char)))
  (val-sizes (* size-t)))

(def-with-errptr rocksdb-transaction-multi-get-for-update-cf void
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (cfs (* (* rocksdb-column-family-handle)))
  (nkeys size-t)
  (keys (* (* unsigned-char)))
  (key-sizes (* size-t))
  (vals (* (* unsigned-char)))
  (val-sizes (* size-t)))

(def-with-errptr rocksdb-transactiondb-get (* unsigned-char)
  (txn-db (* rocksdb-transactiondb))
  (opts (* rocksdb-readoptions))
  (key (* unsigned-char))
  (klen size-t)
  (vlen (* size-t)))

(def-with-errptr rocksdb-transactiondb-get-pinned (* rocksdb-pinnableslice)
  (txn-db (* rocksdb-transactiondb))
  (opts (* rocksdb-readoptions))
  (key (* unsigned-char))
  (klen size-t))

(def-with-errptr rocksdb-transactiondb-get-cf (* unsigned-char)
  (txn-db (* rocksdb-transactiondb))
  (opts (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (klen size-t)
  (vlen (* size-t)))

(def-with-errptr rocksdb-transactiondb-get-pinned-cf (* rocksdb-pinnableslice)
  (txn-db (* rocksdb-transactiondb))
  (opts (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (klen size-t))

(def-with-errptr rocksdb-transactiondb-multi-get-cf void
  (txn-db (* rocksdb-transactiondb))
  (opts (* rocksdb-readoptions))
  (cfs (* (* rocksdb-column-family-handle)))
  (nkeys size-t)
  (keys (* (* unsigned-char)))
  (key-sizes (* size-t))
  (vals (* (* unsigned-char)))
  (val-sizes (* size-t)))

(def-with-errptr rocksdb-transaction-put void
  (txn (* rocksdb-transaction))
  (key (* unsigned-char))
  (klen size-t)
  (val (* unsigned-char))
  (vlen size-t))

(def-with-errptr rocksdb-transaction-put-cf void
  (txn (* rocksdb-transaction))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (klen size-t)
  (val (* unsigned-char))
  (vlen size-t))

(def-with-errptr rocksdb-transactiondb-put void
  (txn-db (* rocksdb-transactiondb))
  (opts (* rocksdb-writeoptions))
  (key (* unsigned-char))
  (klen size-t)
  (val (* unsigned-char))
  (vlen size-t))

(def-with-errptr rocksdb-transactiondb-put-cf void
  (txn-db (* rocksdb-transactiondb))
  (opts (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (klen size-t)
  (val (* unsigned-char))
  (vlen size-t))

(def-with-errptr rocksdb-transactiondb-write void
  (txn-db (* rocksdb-transactiondb))
  (opts (* rocksdb-writeoptions))
  (batch (* rocksdb-writebatch)))

(def-with-errptr rocksdb-transaction-merge void
  (txn (* rocksdb-transaction))
  (key (* unsigned-char))
  (klen size-t)
  (val (* unsigned-char))
  (vlen size-t))

(def-with-errptr rocksdb-transaction-merge-cf void
  (txn (* rocksdb-transaction))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (klen size-t)
  (val (* unsigned-char))
  (vlen size-t))

(def-with-errptr rocksdb-transactiondb-merge void
  (txn (* rocksdb-transactiondb))
  (opts (* rocksdb-writeoptions))
  (key (* unsigned-char))
  (klen size-t)
  (val (* unsigned-char))
  (vlen size-t))

(def-with-errptr rocksdb-transactiondb-merge-cf void
  (txn (* rocksdb-transactiondb))
  (opts (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (klen size-t)
  (val (* unsigned-char))
  (vlen size-t))

(defar rocksdb-transactiondb-create-iterator (* rocksdb-iterator)
  (txndb (* rocksdb-transactiondb))
  (opts (* rocksdb-readoptions)))

(defar rocksdb-transactiondb-create-iterator-cf (* rocksdb-iterator)
  (txndb (* rocksdb-transactiondb))
  (opts (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle)))

(defar rocksdb-transactiondb-close void
  (tdb (* rocksdb-transactiondb)))

(def-with-errptr rocksdb-transactiondb-flush void
  (txndb (* rocksdb-transactiondb))
  (opts (* rocksdb-flushoptions)))

(def-with-errptr rocksdb-transactiondb-flush-cf void
  (txndb (* rocksdb-transactiondb))
  (opts (* rocksdb-flushoptions))
  (cf (* rocksdb-column-family-handle)))

(def-with-errptr rocksdb-transactiondb-flush-cfs void
  (txndb (* rocksdb-transactiondb))
  (opts (* rocksdb-flushoptions))
  (cfs (* (* rocksdb-column-family-handle)))
  (ncfs int))

(def-with-errptr rocksdb-transactiondb-flush-wal void
  (txndb (* rocksdb-transactiondb))
  (sync unsigned-char))

(def-with-errptr rocksdb-transactiondb-checkpoint-object-create (* rocksdb-checkpoint)
  (txn-db (* rocksdb-transactiondb)))

(def-with-errptr rocksdb-optimistictransactiondb-open (* rocksdb-optimistictransactiondb)
  (opts (* rocksdb-options))
  (name c-string))

;; https://github.com/facebook/rocksdb/wiki/Checkpoints
(def-with-errptr rocksdb-checkpoint-object-create (* rocksdb-checkpoint)
  (db (* rocksdb)))

(def-with-errptr rocksdb-checkpoint-create void
  (checkpoint (* rocksdb-checkpoint))
  (checkpoint-dir c-string)
  (log-size-for-flush (unsigned 64)))

(defar rocksdb-checkpoint-object-destroy void
  (checkpoint (* rocksdb-checkpoint)))

(def-with-errptr rocksdb-optimistictransactiondb-open-column-families (* rocksdb-optimistictransactiondb)
  (opts (* rocksdb-options))
  (name c-string)
  (ncfs int)
  (cf-names (* c-string))
  (cf-opts (* (* rocksdb-options)))
  (cf-handles (* (* rocksdb-column-family-handle))))

(defar rocksdb-optimistictransactiondb-get-base-db (* rocksdb)
  (otxn-db (* rocksdb-optimistictransactiondb)))

(defar rocksdb-optimistictransactiondb-close-base-db void
  (base-db (* rocksdb)))

(defar rocksdb-optimistictransaction-begin (* rocksdb-transaction)
  (otxn-db (* rocksdb-optimistictransactiondb))
  (wopts (* rocksdb-writeoptions))
  (otxn-opts (* rocksdb-optimistictransaction-options))
  (old-txn (* rocksdb-transaction)))

(def-with-errptr rocksdb-optimistictransactiondb-write void
  (otxn-db (* rocksdb-optimistictransactiondb))
  (wopts (* rocksdb-writeoptions))
  (batch (* rocksdb-writebatch)))

(defar rocksdb-optimistictransactiondb-close void
  (otxn-db (* rocksdb-optimistictransactiondb)))

(def-with-errptr rocksdb-optimistictransactiondb-checkpoint-object-create (* rocksdb-checkpoint)
  (otxn-db (* rocksdb-optimistictransactiondb)))

;;; Perfcontext
(defar rocksdb-set-perf-level void (val int))

(defar rocksdb-perfcontext-create (* rocksdb-perfcontext))

(defar rocksdb-perfcontext-reset void (ctx (* rocksdb-perfcontext)))

(defar rocksdb-perfcontext-report (* unsigned-char) 
  (context (* rocksdb-perfcontext))
  (exclude-zero-counters unsigned-char))

(defar rocksdb-perfcontext-metric unsigned-long
  (context (* rocksdb-perfcontext)) (metric int))

(defar rocksdb-perfcontext-destroy void (ctx (* rocksdb-perfcontext)))

;;; Filter Policy
(defar rocksdb-filterpolicy-destroy void (self (* rocksdb-filterpolicy)))

(defar rocksdb-filterpolicy-create-bloom (* rocksdb-filterpolicy)
  (bits-per-key double))

(defar rocksdb-filterpolicy-create-bloom-full (* rocksdb-filterpolicy)
  (bits-per-key double))

(defar rocksdb-filterpolicy-create-ribbon (* rocksdb-filterpolicy)
  (bloom-equivalent-bits-per-key double))

(defar rocksdb-filterpolicy-create-ribbon-hybrid (* rocksdb-filterpolicy)
  (bloom-equivalent-bits-per-key double)
  (bloom-before-level int))

;;; Snapshot
(defar rocksdb-create-snapshot (* rocksdb-snapshot)
  (db (* rocksdb)))

(defar rocksdb-snapshot-get-sequence-number (unsigned 64)
  (snapshot (* rocksdb-snapshot)))

(defar rocksdb-release-snapshot void
  (db (* rocksdb))
  (snapshot (* rocksdb-snapshot)))

;;; Pinnable Slices
(def-with-errptr rocksdb-get-pinned (* rocksdb-pinnableslice)
  (db (* rocksdb))
  (opts (* rocksdb-readoptions))
  (key (* unsigned-char))
  (keylen size-t))

(def-with-errptr rocksdb-get-pinned-cf (* rocksdb-pinnableslice)
  (db (* rocksdb))
  (opts (* rocksdb-readoptions))
  (cf-handle (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (keylen size-t))

(defar rocksdb-pinnableslice-destroy void (v (* rocksdb-pinnableslice)))

(defar rocksdb-pinnableslice-value (* unsigned-char) 
  (ty (* rocksdb-pinnableslice))
  (vlen (* size-t)))

;;; Memory Consumers
(defar rocksdb-memory-consumers-create (* rocksdb-memory-consumers))
(defar rocksdb-memory-consumers-add-db void 
  (consumers (* rocksdb-memory-consumers))
  (db (* rocksdb)))
(defar rocksdb-memory-consumers-add-cache void 
  (consumers (* rocksdb-memory-consumers))
  (cache (* rocksdb-cache)))
(defar rocksdb-memory-consumers-destroy void (consumers (* rocksdb-memory-consumers)))

(def-with-errptr rocksdb-approximate-memory-usage-create (* rocksdb-memory-usage)
  (consumers (* rocksdb-memory-consumers)))
(defar rocksdb-approximate-memory-usage-destroy void (usage (* rocksdb-memory-usage)))
(defar rocksdb-approximate-memory-usage-get-mem-table-total unsigned-long 
  (usage (* rocksdb-memory-usage)))
(defar rocksdb-approximate-memory-usage-get-mem-table-unflushed unsigned-long 
  (usage (* rocksdb-memory-usage)))
(defar rocksdb-approximate-memory-usage-get-mem-table-readers-total unsigned-long 
  (usage (* rocksdb-memory-usage)))
(defar rocksdb-approximate-memory-usage-get-cache-total unsigned-long 
  (usage (* rocksdb-memory-usage)))

;;; DbPath
(defar rocksdb-dbpath-create (* rocksdb-dbpath)
  (path c-string)
  (target-size (unsigned 64)))
(defar rocksdb-dbpath-destroy (* rocksdb-dbpath)
  (path (* rocksdb-dbpath)))
