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

(def-with-errptr rocksdb-put 
      void 
      (db (* rocksdb))
     (options (* rocksdb-writeoptions))
     (key (* char))
     (keylen size-t) 
     (val (* char))
     (vallen size-t))

(def-with-errptr rocksdb-get 
  (* char)
  (db (* rocksdb))
  (options (* rocksdb-readoptions))
  (key (* char))
  (keylen size-t) 
  (vallen (* size-t)))

(def-with-errptr rocksdb-delete 
  void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (key (* char))
  (keylen size-t))

(def-with-errptr rocksdb-merge 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t))

(def-with-errptr rocksdb-merge-cf 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t))

(def-with-errptr rocksdb-write 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (batch (* rocksdb-writebatch)))

(def-with-errptr rocksdb-get-cf 
  (* char)
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* char))
  (keylen size-t)
  (vallen (* size-t)))

(define-alien-routine rocksdb-multi-get void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (num-keys size-t)
  (keys-list (array c-string))
  (keys-list-sizes (array size-t))
  (values-list (array c-string))
  (values-list-sizes (array size-t))
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

(define-alien-routine rocksdb-cache-create-lru (* rocksdb) (capacity unsigned-int))

(def-with-errptr rocksdb-flush void 
  (db (* rocksdb))
  (options (* rocksdb-flushoptions)))

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
  (* rocksdb-column-family-handle))

(define-alien-routine rocksdb-column-family-handle-get-id unsigned-int
  (* rocksdb-column-family-handle))

(define-alien-routine rocksdb-column-family-handle-get-name c-string
  (handle (* rocksdb-column-family-handle))
  (name-len (* size-t)))

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

(def-with-errptr rocksdb-put-cf 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t))

(def-with-errptr rocksdb-delete-cf 
  void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* char))
  (keylen size-t))

(def-with-errptr rocksdb-delete-range-cf 
  void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (start-key (* char))
  (start-key-len size-t)
  (end-key (* char))
  (end-key-len size-t))

(def-with-errptr rocksdb-destroy-db void
  (opts (* rocksdb-options))
  (path c-string))

;;; Iterators
(define-alien-routine rocksdb-create-iterator (* rocksdb-iterator)
      (db (* rocksdb))
      (opt (* rocksdb-readoptions)))
(define-alien-routine rocksdb-iter-destroy void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-seek-to-first void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-valid boolean 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-next void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-prev void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-key (* char)
  (iter (* rocksdb-iterator))
  (klen-ptr (* size-t)))
(define-alien-routine rocksdb-iter-value (* char) 
  (iter (* rocksdb-iterator)) 
  (vlen-ptr (* size-t)))

;;; Backup
(def-with-errptr rocksdb-backup-engine-open
  (* rocksdb-backup-engine)
  (opts (* rocksdb-options))
  (path (* char)))

(def-with-errptr rocksdb-backup-engine-create-new-backup
  void
  (be (* rocksdb-backup-engine))
  (db (* rocksdb)))

(def-with-errptr rocksdb-backup-engine-restore-db-from-latest-backup
  void
  (be (* rocksdb-backup-engine))
  (db-dir (* char))
  (wal-dir (* char))
  (res-opts (* rocksdb-restore-options)))

(def-with-errptr rocksdb-backup-engine-restore-db-from-backup
  void
  (be (* rocksdb-backup-engine))
  (db-dir (* char))
  (wal-dir (* char))
  (res-opts (* rocksdb-restore-options))
  (backup-id unsigned-int))

(define-alien-routine rocksdb-backup-engine-close void
  (be (* rocksdb-backup-engine)))

;;; Transactions
(define-alien-routine rocksdb-transaction-begin (* rocksdb-transaction)
  (wopts (* rocksdb-writeoptions))
  (topts (* rocksdb-transaction-options))
  (told (* rocksdb-transaction)))

(def-with-errptr rocksdb-transactiondb-open
  (* rocksdb-transactiondb)
  (opts (* rocksdb-options))
  (topts (* rocksdb-transactiondb-options))
  (name (* char)))

(define-alien-routine rocksdb-transactiondb-close void
  (tdb (* rocksdb-transactiondb)))

;;; BlobDB
