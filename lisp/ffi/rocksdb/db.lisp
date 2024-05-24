(in-package :rocksdb)

;;; DB
(def-with-errptr rocksdb-open (* rocksdb)
  (opt (* rocksdb-options))
  (name c-string))

(define-alien-routine rocksdb-close void 
      (db (* rocksdb)))

(define-alien-routine rocksdb-cancel-all-background-work void 
  (db (* rocksdb))
  (wait boolean))

(define-alien-routine rocksdb-disable-manual-compaction void
  (db (* rocksdb)))

(define-alien-routine rocksdb-enable-manual-compaction void
  (db (* rocksdb)))

(export '(rocksdb-close rocksdb-cancel-all-background-work
          rocksdb-enable-manual-compaction rocksdb-disable-manual-compaction))

(def-with-errptr rocksdb-put 
  void 
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (key (* unsigned-char))
  (keylen size-t) 
  (val (* unsigned-char))
  (vallen size-t))

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

(def-with-errptr rocksdb-write 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (batch (* rocksdb-writebatch)))

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

;; NOTE 2023-12-19: only the VOID-returning functions in the multi-
;; family perform parallel IO:
;; https://github.com/facebook/rocksdb/wiki/MultiGet-Performance
(define-alien-routine rocksdb-multi-get void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (num-keys size-t)
  (keys-list (array c-string))
  (keys-list-sizes (array size-t))
  (values-list (array c-string))
  (values-list-sizes (array size-t))
  (errs (array rocksdb-errptr)))

(define-alien-routine rocksdb-multi-get-with-ts void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (num-keys size-t)
  (keys-list (array c-string))
  (keys-list-sizes (array size-t))
  (values-list (array c-string))
  (values-list-sizes (array size-t))
  (ts-list (array c-string))
  (ts-list-sizes (array size-t))
  (errs (array rocksdb-errptr)))

(define-alien-routine rocksdb-multi-get-cf void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (cfs (array rocksdb-column-family-handle))
  (num-keys size-t)
  (keys-list (array c-string))
  (keys-list-sizes (array size-t))
  (values-list (array c-string))
  (values-list-sizes (array size-t))
  (errs (array rocksdb-errptr)))

(define-alien-routine rocksdb-multi-get-cf-with-ts void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (cfs (array rocksdb-column-family-handle))
  (num-keys size-t)
  (keys-list (array c-string))
  (keys-list-sizes (array size-t))
  (values-list (array c-string))
  (values-list-sizes (array size-t))
  (ts-list (array c-string))
  (ts-list-sizes (array size-t))
  (errs (array rocksdb-errptr)))

(export '(rocksdb-multi-get rocksdb-multi-get-cf
          rocksdb-multi-get-with-ts rocksdb-multi-get-cf-with-ts))

(define-alien-routine rocksdb-cache-create-lru (* rocksdb-cache) (capacity size-t))

(export '(rocksdb-cache-create-lru))

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

(define-alien-routine rocksdb-delete-file void
  (db (* rocksdb))
  (name c-string))

(define-alien-routine rocksdb-livefile (* rocksdb-livefiles)
  (db (* rocksdb))
  (name c-string))

(export '(rocksdb-delete-file rocksdb-livefile))

;; return NULL if prop name is unknown, else return pointer to
;; malloc-ed null-term value.
(define-alien-routine rocksdb-property-value c-string
  (db (* rocksdb))
  (propname c-string))

;; return 0 on success, else -1
(define-alien-routine rocksdb-property-int int
  (db (* rocksdb))
  (propname c-string))

(define-alien-routine rocksdb-property-value-cf c-string
  (db (* rocksdb))
  (cf (* rocksdb-column-family-handle))
  (propname c-string))

(define-alien-routine rocksdb-property-int-cf int
  (db (* rocksdb))
  (cf (* rocksdb-column-family-handle))
  (propname c-string))

(export '(rocksdb-property-value rocksdb-property-value-cf rocksdb-property-int rocksdb-property-int-cf))    

;;; CF
(def-with-errptr rocksdb-create-column-family 
  (* rocksdb-column-family-handle)
  (db (* rocksdb))
  (column-family-options (* rocksdb-options))
  (column-family-name c-string))

(def-with-errptr rocksdb-create-column-families 
  (array rocksdb-column-family-handle)
  (db (* rocksdb))
  (column-family-options (* rocksdb-options))
  (num-column-familes int)
  (column-family-names (array c-string))
  (lencfs (* size-t)))

(define-alien-routine rocksdb-create-column-families-destroy void
  (list (array rocksdb-column-family-handle)))

(define-alien-routine rocksdb-column-family-handle-destroy void
  (cf (* rocksdb-column-family-handle)))

(define-alien-routine rocksdb-column-family-handle-get-id unsigned-int
  (cf (* rocksdb-column-family-handle)))

(define-alien-routine rocksdb-column-family-handle-get-name c-string
  (handle (* rocksdb-column-family-handle))
  (name-len (* size-t)))

(export '(rocksdb-create-column-families-destroy rocksdb-column-family-handle-destroy
          rocksdb-column-family-handle-get-id rocksdb-column-family-handle-get-name))

(def-with-errptr rocksdb-drop-column-family 
  void
  (db (* rocksdb))
  (handle (* rocksdb-column-family-handle)))

(def-with-errptr rocksdb-open-column-families 
  (* rocksdb)
  (options (* rocksdb-options))
  (name c-string)
  (num-column-families int)
  (column-family-names (array c-string))
  (column-family-options (array rocksdb-options))
  (column-family-handles (array rocksdb-column-family-handle)))

(def-with-errptr rocksdb-list-column-families 
  (array c-string)
  (opt (* rocksdb-options))
  (name c-string)
  (lencf (* size-t)))

(define-alien-routine rocksdb-list-column-families-destroy void
  (list (array c-string))
  (len size-t))

(export '(rocksdb-list-column-families-destroy))

(def-with-errptr rocksdb-put-cf 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (keylen size-t)
  (val (* unsigned-char))
  (vallen size-t))

(def-with-errptr rocksdb-delete-cf 
  void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (keylen size-t))

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
(define-alien-routine rocksdb-create-iterator (* rocksdb-iterator)
  (db (* rocksdb))
  (opt (* rocksdb-readoptions)))

(define-alien-routine rocksdb-create-iterator-cf (* rocksdb-iterator)
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle)))

(define-alien-routine rocksdb-iter-destroy void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-seek-to-first void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-seek-to-last void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-seek void 
  (iter (* rocksdb-iterator))
  (k (* unsigned-char))
  (klen size-t))
(define-alien-routine rocksdb-iter-seek-for-prev void 
  (iter (* rocksdb-iterator))
  (k (* unsigned-char))
  (klen size-t))
(define-alien-routine rocksdb-iter-valid boolean 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-next void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-prev void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-key (* unsigned-char)
  (iter (* rocksdb-iterator))
  (klen-ptr (* size-t)))
(define-alien-routine rocksdb-iter-value (* unsigned-char) 
  (iter (* rocksdb-iterator)) 
  (vlen-ptr (* size-t)))
(define-alien-routine rocksdb-iter-timestamp (* unsigned-char) 
  (iter (* rocksdb-iterator))
  (tslen (* size-t)))
(def-with-errptr rocksdb-iter-get-error void (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-wal-iter-next void (iter (* rocksdb-wal-iterator)))
(define-alien-routine rocksdb-wal-iter-valid unsigned-char (iter (* rocksdb-wal-iterator)))
(def-with-errptr rocksdb-wal-iter-status unsigned-char (iter (* rocksdb-wal-iterator)))
(define-alien-routine rocksdb-wal-iter-get-batch (* rocksdb-writebatch)
  (iter (* rocksdb-wal-iterator))
  (seq (* (unsigned 64))))
(define-alien-routine rockdsb-get-latest-sequence-number (unsigned 64) (db (* rocksdb)))
(define-alien-routine rocksdb-wal-iter-destroy void
  (iter (* rocksdb-wal-iterator)))

(export '(rocksdb-create-iterator rocksdb-iter-destroy rocksdb-iter-seek-to-first rocksdb-iter-valid
          rocksdb-iter-seek-to-last rocksdb-iter-seek rocksdb-iter-seek-for-prev
          rocksdb-iter-next rocksdb-iter-prev rocksdb-iter-key rocksdb-iter-value rocksdb-create-iterator-cf
          rocksdb-iter-timestamp rocksdb-iter-get-error rocksdb-wal-iter-next rocksdb-wal-iter-valid
          rocksdb-wal-iter-get-batch rocksdb-get-latest-sequence-number rocksdb-wal-iter-destroy))


;;; Backup
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

(define-alien-routine rocksdb-backup-engine-close void
  (be (* rocksdb-backup-engine)))

(export '(rocksdb-backup-engine-close))

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
  (cf-names (array c-string))
  (cf-opfs (* rocksdb-options))
  (cf-handles (array (* rocksdb-column-family-handle))))

(define-alien-routine rocksdb-transactiondb-create-snapshot (* rocksdb-snapshot)
  (txn-db (* rocksdb-transactiondb))
  (snapshot (* rocksdb-snapshot)))

(define-alien-routine rocksdb-transactiondb-release-snapshot void
  (txn-db (* rocksdb-transactiondb))
  (snapshot (* rocksdb-snapshot)))

(define-alien-routine rocksdb-transactiondb-property-value c-string
  (db (* rocksdb-transactiondb))
  (propname c-string))

(define-alien-routine rocksdb-transactiondb-property-int int
  (db (* rocksdb-transactiondb))
  (propname c-string)
  (out-val (unsigned 64)))

(define-alien-routine rocksdb-transactiondb-get-base-db (* rocksdb)
  (txn-db (* rocksdb-transactiondb)))

(define-alien-routine rocksdb-transactiondb-get-close-db void
  (base-db (* rocksdb)))

(define-alien-routine rocksdb-transaction-begin (* rocksdb-transaction)
  (wopts (* rocksdb-writeoptions))
  (topts (* rocksdb-transaction-options))
  (told (* rocksdb-transaction)))

(define-alien-routine rocksdb-transactiondb-get-prepared-transactions (array (* rocksdb-transaction))
  (txn-db (* rocksdb-transactiondb))
  (cnt (* size-t)))

(def-with-errptr rocksdb-transaction-set-name void
  (txn (* rocksdb-transaction))
  (name c-string)
  (name-len size-t))

(define-alien-routine rocksdb-transaction-get-name c-string
  (txn (* rocksdb-transaction))
  (name-len (* size-t)))

(def-with-errptr rocksdb-transaction-prepare void
  (txn (* rocksdb-transaction)))

(def-with-errptr rocksdb-transaction-commit void
  (txn (* rocksdb-transaction)))

(def-with-errptr rocksdb-transaction-rollback void
  (txn (* rocksdb-transaction)))

(define-alien-routine rocksdb-transaction-set-savepoint void
  (txn (* rocksdb-transaction)))

(def-with-errptr rocksdb-transaction-rollback-to-savepoint void
  (txn (* rocksdb-transaction)))

(define-alien-routine rocksdb-transaction-destroy void
  (txn (* rocksdb-transaction)))

(define-alien-routine rocksdb-transaction-get-writebach-wi (* rocksdb-writebatch-wi)
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

(define-alien-routine rocksdb-transaction-create-iterator (* rocksdb-iterator)
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions)))

(define-alien-routine rocksdb-transaction-create-iterator-cf (* rocksdb-iterator)
  (txn (* rocksdb-transaction))
  (opts (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle)))

(define-alien-routine rocksdb-transactiondb-create-iterator (* rocksdb-iterator)
  (txndb (* rocksdb-transactiondb))
  (opts (* rocksdb-readoptions)))

(define-alien-routine rocksdb-transactiondb-create-iterator-cf (* rocksdb-iterator)
  (txndb (* rocksdb-transactiondb))
  (opts (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle)))

(define-alien-routine rocksdb-transactiondb-close void
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
  (cfs (array (* rocksdb-column-family-handle)))
  (ncfs int))

(def-with-errptr rocksdb-transactiondb-flush-wal void
  (txndb (* rocksdb-transactiondb))
  (sync unsigned-char))

(def-with-errptr rocksdb-transactiondb-checkpoint-object-create (* rocksdb-checkpoint)
  (txn-db (* rocksdb-transactiondb)))

(def-with-errptr rocksdb-optimistictransactiondb-open (* rocksdb-optimistictransactiondb)
  (opts (* rocksdb-options))
  (name c-string))

(def-with-errptr rocksdb-optimistictransactiondb-open-column-families (* rocksdb-optimistictransactiondb)
  (opts (* rocksdb-options))
  (name c-string)
  (ncfs int)
  (cf-names (array c-string))
  (cf-opts (array (* rocksdb-options)))
  (cf-handles (array (* rocksdb-column-family-handle))))

(define-alien-routine rocksdb-optimistictransactiondb-get-base-db (* rocksdb)
  (otxn-db (* rocksdb-optimistictransactiondb)))

(define-alien-routine rocksdb-optimistictransactiondb-close-base-db void
  (base-db (* rocksdb)))

(define-alien-routine rocksdb-optimistictransaction-begin (* rocksdb-transaction)
  (otxn-db (* rocksdb-optimistictransactiondb))
  (wopts (* rocksdb-writeoptions))
  (otxn-opts (* rocksdb-optimistictransaction-options))
  (old-txn (* rocksdb-transaction)))

(def-with-errptr rocksdb-optimistictransactiondb-write void
  (otxn-db (* rocksdb-optimistictransactiondb))
  (wopts (* rocksdb-writeoptions))
  (batch (* rocksdb-writebatch)))

(define-alien-routine rocksdb-optimistictransactiondb-close void
  (otxn-db (* rocksdb-optimistictransactiondb)))

(def-with-errptr rocksdb-optimistictransactiondb-checkpoint-object-create (* rocksdb-checkpoint)
  (otxn-db (* rocksdb-optimistictransactiondb)))

(export '(rocksdb-transaction-begin rocksdb-transaction-close rocksdb-transactiondb-create-snapshot
          rocksdb-transactiondb-release-snapshot rocksdb-transactiondb-property-value
          rocksdb-transactiondb-property-int rocksdb-transactiondb-get-base-db
          rocksdb-transactiondb-get-close-db rocksdb-transaction-get-name
          rocksdb-transaction-set-savepoint rocksdb-transaction-destroy
          rocksdb-transaction-create-iterator rocksdb-transaction-create-iterator-cf
          rocksdb-transactiondb-create-iterator rocksdb-transactiondb-create-iterator-cf
          rocksdb-optimistictransactiondb-get-base-db rocksdb-optimistictransactiondb-close-base-db
          rocksdb-optimistictransaction-begin rocksdb-optimistictransactiondb-close
          rocksdb-transactiondb-flush-wal))

;;; Perfcontext
(define-alien-routine rocksdb-set-perf-level void (val int))

(define-alien-routine rocksdb-perfcontext-create (* rocksdb-perfcontext))

(define-alien-routine rocksdb-perfcontext-reset void (ctx (* rocksdb-perfcontext)))

(define-alien-routine rocksdb-perfcontext-report (* unsigned-char) 
  (context (* rocksdb-perfcontext))
  (exclude-zero-counters unsigned-char))

(define-alien-routine rocksdb-perfcontext-metric unsigned-long
  (context (* rocksdb-perfcontext)) (metric int))

;; TODO 2024-05-24: causes compile error - pass-by-struct not supported
;; (define-alien-routine rocksdb-perfcontext-destroy void (* rocksdb-perfcontext))

(export '(rocksdb-perfcontext-reset rocksdb-perfcontext-report
          rocksdb-perfcontext-metric rocksdb-perfcontext-destroy rocksdb-set-perf-level))

;;; Filter Policy
(define-alien-routine rocksdb-filterpolicy-destroy void (self (* rocksdb-filterpolicy)))

(define-alien-routine rocksdb-filterpolicy-create-bloom (* rocksdb-filterpolicy)
  (bits-per-key double))

(define-alien-routine rocksdb-filterpolicy-create-bloom-full (* rocksdb-filterpolicy)
  (bits-per-key double))

(define-alien-routine rocksdb-filterpolicy-create-ribbon (* rocksdb-filterpolicy)
  (bloom-equivalent-bits-per-key double))

(define-alien-routine rocksdb-filterpolicy-create-ribbon-hybrid (* rocksdb-filterpolicy)
  (bloom-equivalent-bits-per-key double)
  (bloom-before-level int))

(export '(rocksdb-filterpolicy-destroy rocksdb-filterpolicy-create-bloom rocksdb-filterpolicy-create-bloom-full
          rocksdb-filterpolicy-create-ribbon rocksdb-filterpolicy-create-ribbon-hybrid))

;;; Snapshot
(define-alien-routine rocksdb-create-snapshot (* rocksdb-snapshot)
  (db (* rocksdb)))

(define-alien-routine rocksdb-snapshot-get-sequence-number (unsigned 64)
  (snapshot (* rocksdb-snapshot)))

(define-alien-routine rocksdb-release-snapshot void
  (db (* rocksdb))
  (snapshot (* rocksdb-snapshot)))

(export '(rocksdb-create-snapshot rocksdb-snapshot-get-sequence-number rocksdb-release-snapshot))
