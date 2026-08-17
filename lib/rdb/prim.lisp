;;; rdb/prim.lisp --- Primitive RocksDB Operations

;; Basic wrappers where KV handling is irrelevant.

;;; Code:
(in-package :rdb)

;;; Callbacks
;;;; Merge Ops
(defun create-index-merge-op ()
  (with-alien ((state (* t))
               (destructor (* rocksdb-destructor-function) (alien-sap (alien-callable-function 'rocksdb-destructor)))
               (full-merge (* rocksdb-full-merge-function) (alien-sap (alien-callable-function 'rocksdb-index-full-merge)))
               (partial-merge (* rocksdb-partial-merge-function) (alien-sap (alien-callable-function 'rocksdb-index-partial-merge)))
               (delete-value (* rocksdb-delete-value-function) (alien-sap (alien-callable-function 'rocksdb-delete-value)))
               (name (* rocksdb-name-function) (alien-sap (alien-callable-function 'rocksdb-index-merge-name))))
    (rocksdb-mergeoperator-create state destructor full-merge partial-merge delete-value name)))

(defun create-concat-merge-op ()
  (with-alien ((state (* t))
               (destructor (* rocksdb-destructor-function) (alien-sap (alien-callable-function 'rocksdb-destructor)))
               (full-merge (* rocksdb-full-merge-function) (alien-sap (alien-callable-function 'rocksdb-concat-full-merge)))
               (partial-merge (* rocksdb-partial-merge-function) (alien-sap (alien-callable-function 'rocksdb-concat-partial-merge)))
               (delete-value (* rocksdb-delete-value-function) (alien-sap (alien-callable-function 'rocksdb-delete-value)))
               (name (* rocksdb-name-function) (alien-sap (alien-callable-function 'rocksdb-concat-merge-name))))
    (rocksdb-mergeoperator-create state destructor full-merge partial-merge delete-value name)))

;;;; Prefix Ops
(defun create-fixed-prefix-op (n)
  (rocksdb-slicetransform-create-fixed-prefix n))
;;;; Comparators
(defun lisp-comparator ()
  "Return a ROCKSDB-COMPARATOR pointer which can be set as the default for a DB or CF."
  ;; TODO 2026-08-16: 
)

;;;; Logger
(defun create-default-logger-callback (&optional (level 0))
  (rocksdb-logger-create-callback-logger 
   level
   ;; static address to alien-callable
   (alien-sap (alien-callable-function 'rocksdb-log-default)) 
   nil))

;;; Options
(defun %load-opts (dir)
  (rocksdb::with-latest-options dir (db-opts names cf-opts)
    (values db-opts names cf-opts)))

(defun %get-stats (opt htype)
  (with-alien ((hist (* rocksdb-statistics-histogram-data) (rocksdb-statistics-histogram-data-create)))
    (rocksdb-options-statistics-get-histogram-data opt htype hist)
    hist))

;;; DB
(deftype db-identity () 
  "A RocksDB 'db-identity' is a 36-byte sequence which uniquely identifies a DB instance."
`(octet-vector 36))

(defun get-base-db (db)
  (etypecase db
    ((alien (* rocksdb-transactiondb)) (rocksdb-transactiondb-get-base-db db))
    ((alien (* rocksdb-optimistictransactiondb)) (rocksdb-optimistictransactiondb-get-base-db db))))

(defun list-column-families (path &optional (opts *default-rocksdb-options*))
  (with-errptr* (e 'open-db-error :db path)
    (multiple-value-bind (cfs cflen) (rocksdb-list-column-families opts (namestring path) e)
      (loop for i below cflen collect (deref cfs i)))))
        
(defun %open-db (db-path &optional (opts (rocksdb-options-create)))
  (with-errptr* (err 'open-db-error :db db-path)
    (let* ((db-path (if (pathnamep db-path)
                        (namestring db-path)
                        db-path)))
      (rocksdb-open opts db-path err))))

(defun %close-db (db)
  (when db
    (typecase db
      ((alien (* rocksdb)) (rocksdb-close db))
      ((alien (* rocksdb-transactiondb)) (rocksdb-transactiondb-close db))
      ((alien (* rocksdb-optimistictransactiondb)) (rocksdb-optimistictransactiondb-close db)))))

(defun %destroy-db (path &optional (opt (rocksdb-options-create)))
  (with-errptr* (err 'destroy-db-error :db path)
    (rocksdb-destroy-db opt (namestring (probe-directory path)) err)))

(defun %get-metadata (db &optional cf)
  (if cf
      (rocksdb-get-column-family-metadata-cf db cf)
      (rocksdb-get-column-family-metadata db)))

(defun %flush-db (db &optional wait)
  (with-errptr* (err 'flush-db-error :db db)
    (let ((opts (rocksdb-flushoptions-create)))
      (when wait (rocksdb-flushoptions-set-wait opts wait))
      (rocksdb-flush db opts err))))

(defun %repair-db (name &optional (opts (rocksdb-options-create)))
  (with-errptr* (err 'repair-db-error :name name)
    (rocksdb-repair-db opts name err)))

(defun %ingest-db (db files &optional (opts (rocksdb-ingestexternalfileoptions-create)))
  (let ((flen (length files)))
    (with-errptr* (err 'ingest-db-error)
      (with-alien ((flist (* c-string) (make-alien c-string flen)))
        (loop for f in files
              for i from 0 to flen
              do (setf (deref flist i) (make-alien-string f :null-terminate t)))
        (rocksdb-ingest-external-file db flist flen opts err)))))

(defun %ingest-db-cf (db cf files &optional (opts (rocksdb-ingestexternalfileoptions-create)))
  (let ((flen (length files)))
    (with-errptr* (err 'ingest-db-error)
      (with-alien ((flist (* c-string) (make-alien c-string flen)))
        (loop for f in files
              for i from 0 to flen
              do (setf (deref flist i) (make-alien-string f :null-terminate t)))
        (rocksdb-ingest-external-file-cf db cf flist flen opts err)))))

;;; Column Families
(defun %open-cfs (db-opt name names opts)
  (let ((n (length names)))
    (with-alien ((cf-names (* c-string) (clone-strings names))
                 (cf-opts (* (* rocksdb-options)) (make-alien (* rocksdb-options) n))
                 (cf-handles (* (* rocksdb-column-family-handle)) (make-alien (* rocksdb-column-family-handle) n)))
      (loop for opt in opts
            for i below n
            do (setf (deref cf-opts i) opt))
      (with-errptr* (err 'cf-error :cf name)
        (let ((db (rocksdb-open-column-families db-opt name n cf-names cf-opts cf-handles err)))
          (values db cf-handles))))))

(defun %create-cf (db name &optional (opt (rocksdb-options-create)))
  (with-errptr* (err 'cf-error :db db :cf name)
    (rocksdb-create-column-family db opt name err)))

(defun %destroy-cf (cf)
  (rocksdb-column-family-handle-destroy cf))

(defun %cf-name (cf-handle)
  (multiple-value-bind (name len) (rocksdb-column-family-handle-get-name cf-handle)
    (copy-c-string name (make-string len))))

(defun %cf-id (cf-handle)
  (rocksdb-column-family-handle-get-id cf-handle))

;;; Iterators
(defun %create-iter (db &optional (opt (rocksdb-readoptions-create)))
  (rocksdb-create-iterator db opt))

(defun %create-cf-iter (db cf &optional (opt (rocksdb-readoptions-create)))
  (rocksdb-create-iterator-cf db opt cf))

(defun %transaction-wbwi (self)
  (rocksdb-transaction-get-writebach-wi self))

(defun %transaction-create-iter (txn &optional (opts (rocksdb-readoptions-create)))
  (rocksdb-transaction-create-iterator txn opts))

(defun %transaction-create-iter-cf (txn cf &optional (opts (rocksdb-readoptions-create)))
  (rocksdb-transaction-create-iterator-cf txn opts cf))

(defun %transactiondb-create-iter (txndb &optional (opts (rocksdb-readoptions-create)))
  (rocksdb-transactiondb-create-iterator txndb opts))

(defun %transactiondb-create-iter-cf (txndb cf &optional (opts (rocksdb-readoptions-create)))
  (rocksdb-transactiondb-create-iterator-cf txndb opts cf))

(defun %create-iterators (db opts columns)
  (with-alien ((iters (* (* rocksdb-iterator))
                      (make-alien (* rocksdb-iterator) (length columns)))
               (cfs (* (* rocksdb-column-family-handle)) 
                    (make-alien (* rocksdb-column-family-handle) (length columns))))
    (with-errptr e
      (rocksdb-create-iterators db opts cfs iters e))))

(defun %reset-iter (iter)
  (with-errptr e (rocksdb-iter-refresh iter e)))

(defun %destroy-iter (iter)
  (rocksdb-iter-destroy iter))

;;; Backup DB
(defun %open-backup-engine (opts path)
  (with-errptr* (err 'open-db-error :db path)
    (let ((be-path (if (pathnamep path)
                       (namestring path)
                       path)))
      (rocksdb-backup-engine-open opts be-path err))))

(defun %close-backup-engine (be)
  (rocksdb-backup-engine-close be))

(defun %create-new-backup (be db)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-backup-engine-create-new-backup be db err)))

(defun %restore-from-latest-backup (be db-path backup-path &optional (opt (rocksdb-restore-options-create)))
  (with-errptr* (err 'open-db-error)
    (rocksdb-backup-engine-restore-db-from-latest-backup be db-path backup-path opt err)))

(defun %restore-from-backup (be db-path backup-path backup-id &optional (opt (rocksdb-restore-options-create)))
  (with-errptr* (err 'open-db-error)
    (rocksdb-backup-engine-restore-db-from-backup be db-path backup-path opt backup-id err)))

(defun %backup-info (be)
  (rocksdb-backup-engine-get-backup-info be))

;;; Snapshot
(defun %create-snapshot (db)
  (rocksdb-create-snapshot db))

(defun %release-snapshot (db snapshot)
  (rocksdb-release-snapshot db snapshot))

;;; Env
(defun %destroy-env (env) (rocksdb-env-destroy env))
(defun %rocksdb-env (&optional mem)
  (if mem (rocksdb-create-mem-env) (rocksdb-create-default-env)))

(defun %rocksdb-env-get (key &optional (env (%rocksdb-env)))
  (ecase (keywordicate key)
    (:high-priority-background-threads (rocksdb::rocksdb-env-get-high-priority-background-threads env))
    (:low-priority-background-threads (rocksdb::rocksdb-env-get-low-priority-background-threads env))
    (:bottom-priority-background-threads (rocksdb::rocksdb-env-get-bottom-priority-background-threads env))
    (:background-threads (rocksdb-env-get-background-threads env))))

(defun %rocksdb-env-set (key val &optional (env (%rocksdb-env)))
  (ecase (keywordicate key)
    (:high-priority-background-threads (rocksdb::rocksdb-env-set-high-priority-background-threads env val))
    (:low-priority-background-threads (rocksdb::rocksdb-env-set-low-priority-background-threads env val))
    (:bottom-priority-background-threads (rocksdb::rocksdb-env-set-bottom-priority-background-threads env val))
    (:background-threads (rocksdb-env-set-background-threads env val))))

;;; SST
(defun %create-sst-writer (&optional (env-opts (rocksdb-envoptions-create)) (io-opts (rocksdb-options-create)))
  (rocksdb-sstfilewriter-create env-opts io-opts))

(defun %create-sst-writer-with-comparator (comparator
                                              &optional
                                                (env-opts (rocksdb-envoptions-create))
                                                (io-opts (rocksdb-options-create)))
  (rocksdb-sstfilewriter-create-with-comparator env-opts io-opts comparator))

(defun %sst-filewriter (&optional comparator
                                  (env (rocksdb-envoptions-create))
                                  (opts (rocksdb-options-create)))
  (if comparator
      (%create-sst-writer-with-comparator comparator env opts)
      (%create-sst-writer env opts)))

(defun %finish-sst-writer (writer)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-finish writer err)))

(defun %destroy-sst-writer (writer)
  (rocksdb-sstfilewriter-destroy writer))

(defun %open-sst-writer (writer name)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-open writer name err)))

(defun %sst-file-size (writer)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb::rocksdb-sstfilewriter-file-size writer err)))

;;; Transactions
(defun %open-transactiondb (opts topts name)
  (with-errptr* (e 'open-db-error :db name)
    (rocksdb-transactiondb-open opts topts name e)))

(defun %open-optimistictransactiondb (opts name)
  (with-errptr* (e 'open-db-error :db name)
    (rocksdb-optimistictransactiondb-open opts name e)))

(defun %commit-transaction (txn)
  (with-errptr* (e 'rdb-alien-error)
    (rocksdb-transaction-commit txn e)))

(defun %set-savepoint (txn)
  (rocksdb-transaction-set-savepoint txn))

(defun %rollback-transaction (txn &optional savepoint)
  "Rollback a raw transaction TXN when SAVEPOINT is non-nil only rollback to last
savepoint created with ROCKSDB-TRANSACTION-SET-SAVEPOINT."
  (with-errptr* (e 'rdb-alien-error)
    (if savepoint
        (rocksdb-transaction-rollback-to-savepoint txn e)
        (rocksdb-transaction-rollback txn e))))

(defun %prepare-transaction (txn)
  (with-errptr* (e 'rdb-transaction-error :txn txn)
    (rocksdb-transaction-prepare txn e)))

(defun %transaction-iterator (self &key column (opts (rocksdb-readoptions-create)))
  (if column
      (%transaction-create-iter-cf self column opts)
      (%transaction-create-iter self opts)))

(defun %abort-transaction (self &optional savepoint)
  (%rollback-transaction self savepoint)
  (rocksdb-transaction-destroy self))

(defun %get-prepared-transactions (txn-db)
  "Return an array of prepared ROCKSDB-TRANSACTION pointers from this
transaction-db."
  (with-errptr* (e 'rdb-alien-error :db txn-db)
    (rocksdb-transactiondb-get-prepared-transactions txn-db)))

;;; Checkpoints
(defun %make-checkpoint (db)
  (with-errptr* (e 'rdb-alien-error :db db)
    (rocksdb-checkpoint-object-create db e)))

(defun %create-checkpoint (chk dir &optional log-size-for-flush)
  (with-errptr* (e 'rdb-alien-error)
    (rocksdb-checkpoint-create chk dir log-size-for-flush e)))

;;; Secondary
(defun %open-db-secondary (opts name sname)
  (with-errptr* (e 'open-db-error)
    (rocksdb-open-as-secondary opts name sname e)))

(defun %open-cfs-secondary (opts name sname cf-names cf-opts)
  (with-errptr* (e 'rdb-alien-error)
    (with-alien ((cf-handles (* (* rocksdb-column-family-handle))))
      (rocksdb-open-as-secondary-column-families 
       opts name sname (length cf-names) cf-names cf-opts cf-handles e))))

;;; Read-only
(defun %open-cfs-read-only (opts name cf-names cf-opts &optional err-if-wal)
  (with-errptr* (e 'rdb-alien-error)
    (with-alien ((cf-handles (* (* rocksdb-column-family-handle))))
      (rocksdb-open-for-read-only-column-families 
       opts name (length cf-names) cf-names cf-opts cf-handles err-if-wal e))))

;;; TTL
(defun %open-cfs-with-ttl (opts name cf-names cf-opts ttls)
    (with-errptr* (e 'rdb-alien-error)
      (with-alien ((cf-handles (* (* rocksdb-column-family-handle))))
        (rocksdb-open-column-families-with-ttl 
         opts name (length cf-names) cf-names cf-opts cf-handles ttls e))))

;;; Writebatch/WBWI
(defun %create-wbwi (&optional (reserved-bytes 0) (overwrite-keys 1))
  (rocksdb-writebatch-wi-create reserved-bytes overwrite-keys))
(defun %wbwi-count (self) (rocksdb-writebatch-wi-count self))
(defun %wbwi-clear (wbwi)
  (rocksdb-writebatch-wi-clear wbwi))
(defun %wbwi-save (self)
  (rocksdb-writebatch-wi-set-save-point self))
(defun %writebatch-iter (self)
  (rocksdb-writebatch-iterate self nil nil (alien-callable-function 'rocksdb-delete-value)))
(defun %wbwi-iter (wbwi &key state
                             put
                             (deleted (sb-alien:alien-callable-function 'rocksdb-delete-value)))
  (rocksdb-writebatch-wi-iterate wbwi state put deleted))
(defun %destroy-wbwi (self)
  (rocksdb-writebatch-wi-destroy self))
(defun %wbwi-write (db batch &optional (opts (rocksdb-readoptions-create)))
  (with-errptr e (rocksdb-write-writebatch-wi db opts batch e)))
