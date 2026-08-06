;;; rdb.lisp --- RocksDB Low-level Structures

;; 

;;; Code:
(in-package :rdb)
;;; Iterator
(defstruct rdb-iter 
  (sap nil :type (or null (alien (* rocksdb-iterator)))))

(defaccessor sap ((self rdb-iter)) (rdb-iter-sap self))

(defmethod free ((self rdb-iter))
  (rocksdb-iter-destroy (sap self)))

(defmethod reset ((self rdb-iter) &key)
  (with-errptr e
    (rocksdb-iter-refresh (sap self) e)))

(defmethod iter-valid-p ((self rdb-iter))
  (rocksdb-iter-valid (sap self)))

(defmethod seek-to-first ((self rdb-iter))
  (rocksdb-iter-seek-to-first (rdb-iter-sap self)))

(defmethod seek-to-last ((self rdb-iter))
  (rocksdb-iter-seek-to-last (rdb-iter-sap self)))

(defmethod seek-for-prev ((self rdb-iter) (key vector) &key)
  (rocksdb-iter-seek-for-prev (rdb-iter-sap self) (octets-to-alien key) (length key)))

(defmethod seek ((self rdb-iter) (key simple-vector) &key)
  (rocksdb-iter-seek (rdb-iter-sap self) (octets-to-alien key) (length key)))

(defmethod next ((self rdb-iter))
  (rocksdb-iter-next (rdb-iter-sap self)))

(defmethod prev ((self rdb-iter))
  (rocksdb-iter-prev (rdb-iter-sap self)))

(defmethod skey ((self rdb-iter))
  (with-alien ((klen size-t))
    (let ((key (rocksdb-iter-key (rdb-iter-sap self) (addr klen))))
      (let ((k (make-octets klen)))
        (clone-octets-from-alien key k)
        (values
         k
         klen)))))

(defmethod sval ((self rdb-iter))
  (with-alien ((vlen size-t))     
    (let ((val (rocksdb-iter-value (sap self) (addr vlen))))
      (let ((v (make-octets vlen)))
        (clone-octets-from-alien val v)
        (values
         v
         vlen)))))

(defmethod val ((self rdb-iter)) (sval self))

(defmethod timestamp ((self rdb-iter))
  (with-alien ((tslen size-t))
    (values
     (rocksdb-iter-timestamp (sap self) (addr tslen))
     tslen)))

;;; column family
(defstruct (rdb-cf (:constructor make-rdb-cf (name &key opts sap)))
  "RDB Column Family structure. Contains a name, db-opts,
and a system-area-pointer to the underlying rocksdb_cf_t handle."
  (name "" :type string)
  (opts (default-rdb-opts) :type rdb-opts)
  (sap nil :type (or null (alien (* rocksdb-column-family-handle)))))

(defaccessor column-opts ((self rdb-cf)) (rdb-cf-opts self))
(defaccessor sap ((self rdb-cf)) (rdb-cf-sap self))
(defaccessor name ((self rdb-cf)) (rdb-cf-name self))

(defmethod close-column ((self rdb-cf) &optional error)
  (if-let ((sap (sap self)))
    (setf (sap self) (rocksdb:rocksdb-column-family-handle-destroy sap))
    (when error (simple-rdb-error "column family is already closed."))))

(defmethod merge-key ((self rdb-cf) key val &key db (opts (rocksdb-writeoptions-create)))
  (%merge-cf (sap db) (sap self) key val opts))

;;; rdb-stats
(defstruct (rdb-stats (:constructor make-rdb-stats (&optional sap)))
  (sap nil :type (or null (alien (* rocksdb-statistics-histogram-data)))))

(defaccessor sap ((self rdb-stats)) (rdb-stats-sap self))

;;; metadata
(defstruct rdb-cf-metadata
  (name "default" :type string)
  (size 0 :type fixnum)
  (level-count 7 :type fixnum)
  (file-count 0 :type fixnum)
  (sap nil :type (or null (alien (* rocksdb-column-family-metadata)))))

(defaccessor sap ((self rdb-cf-metadata)) (rdb-cf-metadata-sap self))
(defaccessor name ((self rdb-cf-metadata)) (rdb-cf-metadata-name self))

(defmethod db-metadata ((self rdb-cf-metadata) &optional (level 0))
  (with-slots (sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (make-rdb-level-metadata :sap (rocksdb-column-family-metadata-get-level-metadata sap level)))))

(defmethod print-object ((self rdb-cf-metadata) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (name size level-count file-count) self
      (format stream "~A :size ~A :levels ~A :files ~A" name size level-count file-count))))

(defmethod pull-sap* ((self rdb-cf-metadata))
  (with-slots (name size level-count file-count sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (setf name (rocksdb-column-family-metadata-get-name sap)
              size (rocksdb-column-family-metadata-get-size sap)
              level-count (rocksdb-column-family-metadata-get-level-count sap)
              file-count (rocksdb-column-family-metadata-get-file-count sap)))
    self))

(defstruct rdb-level-metadata
  (level 0 :type fixnum)
  (size 0 :type fixnum)
  (file-count 0 :type fixnum)
  (sap nil :type (or null (alien (* rocksdb-level-metadata)))))

(defaccessor sap ((self rdb-level-metadata)) (rdb-level-metadata-sap self))

(defmethod db-metadata ((self rdb-level-metadata) &optional (file 0))
  (if (null (sap self))
      (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
      (make-rdb-sst-file-metadata :sap (rocksdb-level-metadata-get-sst-file-metadata (sap self) file))))

(defmethod print-object ((self rdb-level-metadata) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (level size file-count) self
      (format stream "~A :size ~A :files ~A" level size file-count))))

(defmethod pull-sap* ((self rdb-level-metadata))
  (with-slots (level size file-count sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (setf level (rocksdb-level-metadata-get-level sap)
              size (rocksdb-level-metadata-get-size sap)
              file-count (rocksdb-level-metadata-get-file-count sap)))
    self))

;; NOTE: we only store the sizes of largest and smallest key, not the
;; keys themselves. This may change in the future.
(defstruct rdb-sst-file-metadata
  (relative-filename "" :type string)
  (directory "" :type string)
  (size 0 :type fixnum)
  (smallestkey 0 :type fixnum)
  (largestkey 0 :type fixnum)
  (sap nil :type (or null (alien (* rocksdb-sst-file-metadata)))))

(defaccessor sap ((self rdb-sst-file-metadata)) (rdb-sst-file-metadata-sap self))

(defmethod print-object ((self rdb-sst-file-metadata) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (relative-filename directory size smallestkey largestkey) self
      (format stream "~A :dir ~A :size ~A :smallest ~A :largest ~A"
              relative-filename directory size smallestkey largestkey))))

(defmethod pull-sap* ((self rdb-sst-file-metadata))
  (with-slots (relative-filename directory size smallestkey largestkey sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (with-alien ((ssize size-t 0)
                     (lsize size-t 0))
          (rocksdb-sst-file-metadata-get-largestkey sap (addr lsize))
          (rocksdb-sst-file-metadata-get-smallestkey sap (addr ssize))
          (setf relative-filename (rocksdb-sst-file-metadata-get-relative-filename sap)
                directory (rocksdb-sst-file-metadata-get-directory sap)
                size (rocksdb-sst-file-metadata-get-size sap)
                largestkey lsize
                smallestkey ssize)))
    self))

;;; Snapshots
(defstruct rdb-snapshot 
  (sap nil :type (or null (alien (* rocksdb-snapshot)))))

(defaccessor sap ((self rdb-snapshot)) (rdb-snapshot-sap self))
(defmethod id ((self rdb-snapshot)) (rocksdb-snapshot-get-sequence-number (sap self)))

;;; Checkpoints
(defstruct rdb-checkpoint 
  (sap nil :type (or null (alien (* rocksdb-checkpoint))))
  path)

(defaccessor sap ((self rdb-checkpoint)) (rdb-checkpoint-sap self))
(defaccessor path ((self rdb-checkpoint)) (rdb-checkpoint-path self))

;;; SST
(defstruct (sst-file-writer (:constructor %make-sst-file-writer (sap)))
  (sap nil :type (or null (alien (* rocksdb-sstfilewriter)))))

(defun make-sst-file-writer (&optional comparator
                                       env-opts
                                       io-opts)
  (let ((env (or env-opts (rocksdb-envoptions-create)))
        (io (or io-opts (rocksdb-options-create))))
  (%make-sst-file-writer
   (if comparator
       (%create-sst-writer-with-comparator comparator env io)
       (%create-sst-writer env io)))))

(defun sst-file-size (writer)
  (declare (sst-file-writer writer))
  (%sst-file-size (sst-file-writer-sap writer)))

(defun open-sst (writer path)
  (declare (sst-file-writer writer))
  (%open-sst-writer (sst-file-writer-sap writer) path))

(defun finish-sst (writer)
  (declare (sst-file-writer writer))
  (%finish-sst-writer (sst-file-writer-sap writer)))

(defun destroy-sst (writer)
  (declare (sst-file-writer writer))
  (with-slots (sap) writer
    (unless (null sap)
      (%destroy-sst-writer sap)
      (setf sap nil))))

(defmethod print-object ((self sst-file-writer) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":size ~A" (when (sst-file-writer-sap self) (sst-file-size self)))))

(defmethod put-key ((self sst-file-writer) key val)
  (%sst-put (sst-file-writer-sap self) key val))

(defmethod put-key ((self sst-file-writer) (key simple-string) (val simple-string))
  (%sst-put-str (sst-file-writer-sap self) key val))

(defmethod delete-key ((self sst-file-writer) key &key)
  (%sst-delete (sst-file-writer-sap self) key))

(defmethod delete-key-ts ((self sst-file-writer) key ts)
  (%sst-delete-ts (sst-file-writer-sap self) key ts))

(defmethod delete-key-range ((self sst-file-writer) start end &key)
  (%sst-delete-range (sst-file-writer-sap self) start end))

(defmethod put-key-ts ((self sst-file-writer) key val ts)
  (%sst-put-ts (sst-file-writer-sap self) key val ts))

;;; rdb
(defstruct rdb
  (name "" :type string)
  (opts (default-rdb-opts) :type rdb-opts)
  (sap nil :type (or null (alien (* rocksdb)))))

(defaccessor sap ((self rdb)) (rdb-sap self))
(defaccessor name ((self rdb)) (rdb-name self))
(defaccessor db ((self rdb)) (sap self))
(defaccessor db-opts ((self rdb)) (rdb-opts self))

(defmethod print-object ((self rdb) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":open ~A" (db-open-p self))))

(defmethod db-open-p ((self rdb))
  (when (sap self) t))

(defmethod db-closed-p ((self rdb))
  (unless (sap self) t))

(defun create-rdb (name &key opts schema open)
  "Construct a new RDB instance from NAME.

OPTS = rdb-opts
CFS = (sequence rdb-cf)
SCHEMA = rdb-schema
OPEN = boolean

CFS are always added before the SCHEMA which is loaded with LOAD-SCHEMA.

When OPEN is non-nil, the database and all column families are opened and
internal sap slots are initialized."
  (when (probe-file name) (log:trace! "attempting to create existing db: ~A" name))
  (let* ((opts (or opts (default-rdb-opts)))
         (obj
           (make-rdb
            :name 
            (string-right-trim '(#\/)
                               (typecase name
                                 (pathname (namestring name))
                                 (string name)
                                 (t (error "invalid NAME: ~S" name))))
            :opts opts)))
    (when schema
      (load-schema obj schema))
    (when open
      (open-db obj))
    obj))

(defmethod backfill-opts ((self rdb) &key full)
  (with-slots (opts) self
    (if full
        (loop for k across *rocksdb-options*
              unless (%rdb-opt-no-getter-p k)
              do (pull-sap opts k))
        (pull-sap* opts))
    (db-opts opts)))

(defmethod open-column ((self rdb) (col rdb-cf) &key)
  (ifret (sap col)
         (setf (sap col) (create-column self col))))

(defmethod create-column ((db rdb) (cf rdb-cf))
  (%create-cf (sap db) (name cf) (sap (column-opts cf))))

(defmacro unless-null-db (slots self &body body)
  `(with-slots (sap ,@slots) ,self
     (unless (null sap)
       ,@body)))

(defmethod free ((cf rdb-cf))
  (with-slots (sap) cf
    (unless (null sap)
      (setf sap (%destroy-cf sap)))))

(defaccessor* db-opt
    ((self rdb) key) (db-opt (db-opts self) (string-downcase key))
    (new (self rdb) key &key push)
  (prog1 (setf (db-opt (db-opts self) (string-downcase key)) new)
    (when push (push-sap (db-opts self) (string-downcase key)))))

(defmethod push-opts ((self rdb))
  (with-slots (opts) self
    (push-sap* opts)))

(defmethod open-db ((self rdb))
  (with-slots (name sap opts) self
    (if sap
        (progn
          (cerror "Ignore and continue" 'open-db-error 
                  :db sap
                  :message "Database is already open")
          sap)
        (setf sap (%open-db name (or (sap opts) (push-opts self)))))))

(defmethod db-prop ((self rdb) (propname string))
  (unless-null-db () self
    (rocksdb-property-value sap propname)))

(defmethod repair-db ((self rdb) &key)
  (%repair-db (rdb-name self)))

(defmethod open-backup-engine ((self rdb) &key path)
  (with-slots (opts) self
    (%open-backup-engine path (sap opts))))

(defmethod backup-db ((self rdb) &key path)
  (unless-null-db (opts) self
    (if (null path)
        (error 'open-backup-engine-error :db sap 
                                         :message "PATH must not be nil when no backups exist")
        (%create-new-backup (open-backup-engine self :path path) sap))))

(defmethod restore-db ((self rdb) (from string) &key id opts)
  (unless-null-db (name) self
    (%restore-from-backup (open-backup-engine self :path from) name from id opts)))

(defmethod snapshot-db ((self rdb))
  (unless-null-db () self
    (make-rdb-snapshot :sap (%create-snapshot sap))))

(defmethod db-metadata ((self rdb) &optional cf)
  (make-rdb-cf-metadata :sap (%get-metadata (rdb-sap self) cf)))

(defmethod db-stats ((self rdb) &optional (htype (rocksdb-statistics-level "all")))
  (make-rdb-stats (%get-stats (sap (rdb-opts self)) htype)))

(defmethod iter ((self rdb) &key cf (opts (rocksdb-readoptions-create)))
  (let ((col (etypecase cf
               (rdb-cf (rdb-cf-sap cf))
               ;; (string (rdb-cf-sap (find-column cf self)))
               (null nil)
               (alien cf))))
    (unless-null-db () self
      (make-rdb-iter 
       :sap (if col
                (%create-cf-iter sap col opts)
                (%create-iter sap opts))))))

(defmethod print-stats ((self rdb) &optional stream)
  (if stream
      (println (rocksdb-options-statistics-get-string (sap (rdb-opts self))) stream)
      (with-output-to-string (s)
        (print-stats self s))))

(defmethod flush-db ((self rdb) &key wait)
  (%flush-db (rdb-sap self) wait))

(defmethod shutdown-db ((self rdb) &key wait)
  (log:trace! "shutting down database" (rdb-name self))
  (when-let ((db (rdb-sap self)))
    (rocksdb-cancel-all-background-work db wait)
    (close-db self)))

(defmethod ingest-db ((self rdb) (files list) &key column (opts (rocksdb-ingestexternalfileoptions-create)))
  (if column
      (%ingest-db-cf (sap self) (sap column) files opts)
      (%ingest-db (sap self) files opts)))

(defmethod close-db ((self rdb) &key &allow-other-keys)
  (with-slots (sap opts) self
    (unless (null sap)
      (%close-db sap)
      (setf (sap self) nil)
      (setf (sap (db-opts self)) (rocksdb-options-destroy (sap (db-opts self)))))))

(defmethod destroy-db ((self rdb))
  ;; close all handles before destruction ensues
  (close-db self)
  (%destroy-db (rdb-name self)))

(defmethods put-key 
  (((self rdb) (key t) (val t))
   (%put-kv
    (rdb-sap self)
    key
    val))
  (((self rdb) (key string) (val string))
   (%put-kv
    (rdb-sap self)
    (sb-ext:string-to-octets key)
    (sb-ext:string-to-octets val))))

(defmethod multi-get ((self rdb) keys &key (data-type 'octet-vector) (opts (rocksdb-readoptions-create)) cf)
  (if cf
      (ecase data-type
        (octet-vector (%multi-get-cf-kv (sap self) keys opts (sap cf)))
        (string (%multi-get-cf-kv-str (sap self) keys opts (sap cf))))
      (ecase data-type
        (octet-vector (%multi-get-kv (sap self) keys opts))
        (string (%multi-get-kv-str (sap self) keys opts)))))

(defmethod get-value ((self rdb) key)
  (%get-kv (sap self) key (rocksdb-readoptions-create)))

(defmethod merge-key ((self rdb) key val &key (opts (rocksdb-writeoptions-create)))
  (%merge-kv (sap self) key val opts))

(defmethod merge-key ((self rdb) (key string) (val string) &key (opts (rocksdb-writeoptions-create)))
  (%merge-kv-str (sap self) key val opts))

;;; Transaction DB
(defstruct rdb-transaction-db 
  (name "" :type string)
  (db-opts (default-rdb-opts) :type rdb-opts)
  (sap nil :type (or null (alien (* rocksdb-transactiondb))))
  ;; struct wrapper?
  (opts (rocksdb-transactiondb-options-create)))

(defaccessor sap ((self rdb-transaction-db)) (rdb-transaction-db-sap self))
(defaccessor db-opts ((self rdb-transaction-db)) (rdb-transaction-db-db-opts self))
(defaccessor name ((self rdb-transaction-db)) (rdb-transaction-db-name self))
(defaccessor db ((self rdb-transaction-db)) (sap self))

(defmethod iter ((self rdb-transaction-db) &key cf (opts (rocksdb-readoptions-create)))
  (let ((col (etypecase cf
               (rdb-cf (rdb-cf-sap cf))
               ;; (string (rdb-cf-sap (find-column cf self)))
               (null nil)
               (alien cf))))
    (unless-null-db () self
      (make-rdb-iter 
       :sap (if col
                (%transactiondb-create-iter-cf sap col opts)
                (%transactiondb-create-iter sap opts))))))

(defstruct rdb-optimistic-transaction-db
  (name "" :type string)
  (db-opts (default-rdb-opts) :type rdb-opts)
  (sap nil :type (or null (alien (* rocksdb-optimistictransactiondb)))))

(defaccessor sap ((self rdb-optimistic-transaction-db)) (rdb-optimistic-transaction-db-sap self))
(defaccessor db-opts ((self rdb-optimistic-transaction-db)) (rdb-optimistic-transaction-db-db-opts self))
(defaccessor name ((self rdb-optimistic-transaction-db)) (rdb-optimistic-transaction-db-name self))
(defaccessor db ((self rdb-optimistic-transaction-db)) (sap self))

(defaccessor* db-opt
    ((self rdb-transaction-db) key) (db-opt (db-opts self) (string-downcase key))
    (new (self rdb-transaction-db) key &key push)
  (prog1 (setf (db-opt (db-opts self) (string-downcase key)) new)
    (when push (push-sap (db-opts self) (string-downcase key)))))

(defmethod push-opts ((self rdb-transaction-db))
  (with-slots (db-opts) self
    (push-sap* db-opts)))

(defaccessor* db-opt
    ((self rdb-optimistic-transaction-db) key) (db-opt (db-opts self) (string-downcase key))
    (new (self rdb-optimistic-transaction-db) key &key push)
  (prog1 (setf (db-opt (db-opts self) (string-downcase key)) new)
    (when push (push-sap (db-opts self) (string-downcase key)))))

(defmethod push-opts ((self rdb-optimistic-transaction-db))
  (with-slots (db-opts) self
    (push-sap* db-opts)))

(defmethod open-db ((self rdb-transaction-db))
  (with-slots (name sap opts db-opts) self
    (if sap
        (progn
          (cerror "Ignore and continue" 'open-db-error 
                  :db sap
                  :message "Database is already open")
          sap)
        (setf sap (%open-transactiondb (or (sap db-opts) (push-opts self)) opts name)))))

(defmethod open-db ((self rdb-optimistic-transaction-db))
  (with-slots (name sap db-opts) self
    (if sap
        (progn
          (cerror "Ignore and continue" 'open-db-error 
                  :db sap
                  :message "Database is already open")
          sap)
        (setf sap (%open-optimistictransactiondb (or (sap db-opts) (push-opts self)) name)))))

(defmethod close-db ((self rdb-transaction-db) &key)
  (when-let ((sap (sap self)))
    (rocksdb-transactiondb-close sap)))

(defmethod close-db ((self rdb-optimistic-transaction-db) &key)
  (when-let ((sap (sap self)))
    (rocksdb-optimistictransactiondb-close sap)))

(defmethods get-val
  (((self rdb-transaction-db) (key string) &key opts cf pinned)
   (let ((sap (sap self))
         (opts (or opts (rocksdb-readoptions-create))))
     (if cf
         (%transactiondb-get-cf-str sap (rdb-cf-sap (find-column cf self)) key opts pinned)
         (%transactiondb-get-kv-str sap key opts pinned))))
  (((self rdb-optimistic-transaction-db) (key string) &key opts cf pinned)
   (let ((sap (sap self))
         (opts (or opts (rocksdb-readoptions-create))))
     (if cf
         (%transactiondb-get-cf-str sap (rdb-cf-sap (find-column cf self)) key opts pinned)
         (%transactiondb-get-kv-str sap key opts pinned))))
  (((self rdb) key &key opts cf pinned)
   (let ((opts (or opts (rocksdb-readoptions-create))))
     (with-slots (sap) self
       (etypecase cf
         (rdb-cf (%get-cf sap (sap cf) key opts pinned))
         (null (%get-kv sap key opts pinned))
         (alien (%get-cf sap cf key opts pinned))))))
  (((self rdb) (key string) &key opts cf pinned)
   (octets-to-string (get-val self (string-to-octets key) :opts (or opts (rocksdb-readoptions-create)) :cf cf :pinned pinned))))

(defmethod get-value ((self rdb-transaction-db) key)
  (%transactiondb-get-kv self key))

;;; Transaction
(defstruct rdb-transaction 
  (sap nil :type (or null (alien (* rocksdb-transaction)))))

(defaccessor sap ((self rdb-transaction)) (rdb-transaction-sap self))
(defaccessor name ((self rdb-transaction)) (%transaction-name (sap self)))
(defmethod free ((self rdb-transaction)) (rocksdb-transaction-destroy (sap self)))

(defmethod transaction-object-p ((self rdb-transaction)) t)

(defmethods make-transaction 
  (((self rdb-transaction-db)
    &key name
    txn
    opts
    write-opts)
   (let ((opts (or opts (rocksdb-transaction-options-create)))
         (write-opts (or write-opts (rocksdb-writeoptions-create))))
   (let ((obj (make-rdb-transaction
               :sap (rocksdb-transaction-begin (sap self) write-opts opts txn))))
     (when name (setf (name obj) name))
     obj)))
  (((self rdb-optimistic-transaction-db)
    &key name
    txn
    opts
    write-opts)
   (let ((opts (or opts (alien-sap (rocksdb-transaction-options-create))))
         (write-opts (or write-opts (rocksdb-writeoptions-create))))
     (let ((obj (make-rdb-transaction
                 :sap (rocksdb-optimistictransaction-begin (sap self) write-opts opts txn))))
       (when name (setf (name obj) name))
       obj))))

(defmethod prepare-transaction ((self rdb-transaction) &key)
  (%prepare-transaction (sap self)))

(defmethod rollback-transaction ((self rdb-transaction) &key savepoint)
  (%rollback-transaction (sap self) savepoint))

(defmethod abort-transaction ((self rdb-transaction) &key)
  (rollback-transaction self)
  (rocksdb-transaction-destroy (sap self)))

(defmethod commit-transaction ((self rdb-transaction) &key)
  (%commit-transaction (sap self)))

(defun rdb-transaction-wbwi (self)
  (rocksdb-transaction-get-writebach-wi (sap self)))

(defmethod iter ((self rdb-transaction) &key cf (opts (rocksdb-readoptions-create)))
  (let ((col (etypecase cf
               (rdb-cf (rdb-cf-sap cf))
               ;; (string (rdb-cf-sap (find-column cf self)))
               (null nil)
               (alien cf))))
    (unless-null-db () self
      (make-rdb-iter 
       :sap (if col
                (%transaction-create-iter-cf sap col opts)
                (%transaction-create-iter sap opts))))))

;;; Secondary DB
(defstruct rdb-secondary-db 
  (sap nil :type (or null (alien (* rocksdb))))
  opts)

(defaccessor sap ((self rdb-secondary-db)) (rdb-secondary-db-sap self))
(defaccessor db-opts ((self rdb-secondary-db)) (rdb-secondary-db-opts self))

(defmethod open-secondary-db ((self rdb) &key path opts)
  (make-rdb-secondary-db 
   :sap (%open-db-secondary opts (name self) path)
   :opts opts))

(defmethod close-secondary-db ((self rdb-secondary-db))
  (rocksdb-close (sap self)))

;;; Backup DB
(defstruct rdb-backup-engine
  (sap nil :type (or null (alien (* rocksdb-backup-engine))))
  opts)

(defaccessor sap ((self rdb-backup-engine)) (rdb-backup-engine-sap self))
(defaccessor db-opts ((self rdb-backup-engine)) (rdb-backup-engine-opts self))

(defmethod open-backup-engine ((self rdb-backup-engine) &key path)
  (setf (sap self) (%open-backup-engine path (db-opts self))))

(defmethod close-backup-engine ((self rdb-backup-engine))
  (%close-backup-engine (sap self)))

(defun rdb-backup-engine-info (be)
  (etypecase be
    (rdb-backup-engine (rocksdb-backup-engine-get-backup-info (sap be)))
    (alien (rocksdb-backup-engine-get-backup-info be))))

;;; Write Batches
(defstruct rdb-writebatch 
  (sap nil :type (or null (alien (* rocksdb-writebatch)))))

(defaccessor sap ((self rdb-writebatch)) (rdb-writebatch-sap self))
(defmethod iter ((self rdb-writebatch) &key)
  (rocksdb-writebatch-iterate (sap self) nil nil (alien-callable-function 'rocksdb-delete-value)))
(defun %writebatch-data (wb)
  (multiple-value-bind (data size) (rocksdb-writebatch-data wb)
    (clone-octets-from-alien data (make-array size :element-type 'octet))))

;; WBWIs consist of a WriteBatch and an Index
(defstruct rdb-wbwi ;; wb reserved overwrite-key data savepoints params
  (sap (%create-wbwi) :type (or null (alien (* rocksdb-writebatch-wi)))))

(defaccessor sap ((self rdb-wbwi)) (rdb-wbwi-sap self))
(defun %wbwi-count (self) (rocksdb-writebatch-wi-count self))
(defun %wbwi-data (wbwi)
  (multiple-value-bind (data size) (rocksdb-writebatch-wi-data wbwi)
    (clone-octets-from-alien data (make-array size :element-type 'octet))))
(defmethod iter ((self rdb-wbwi) &key)
  (rocksdb-writebatch-wi-iterate (sap self) nil nil (sb-alien:alien-callable-function 'rocksdb-delete-value)))
(defun %wbwi-clear (wbwi)
  (rocksdb-writebatch-wi-clear wbwi))
(defun %wbwi-save (self)
  (rocksdb-writebatch-wi-set-save-point self))
(defun %wbwi-ts (self ts)
  (with-errptr e
    (rocksdb-writebatch-wi-update-timestamps 
     self (octets-to-alien ts) (length ts) nil nil e)))
(defun %destroy-wbwi (self)
  (rocksdb-writebatch-wi-destroy self))

(defmethod put-key ((self rdb-wbwi) (key vector) (val vector))
  (rocksdb-writebatch-wi-put 
   (sap self) 
   (cast (octets-to-alien key) (array unsigned-char))
   (length key) 
   (cast (octets-to-alien val) (array unsigned-char))
   (length val)))

(defmethod put-key ((self rdb-wbwi) (key string) (val string))
  (put-key self (string-to-octets key) (string-to-octets val)))

(defmethod get-key ((self rdb-wbwi) (key string) &key)
  (with-errptr e
    (multiple-value-bind (data i)  
        (rocksdb-writebatch-wi-get-from-batch 
         (sap self) 
         (default-rocksdb-options)
         (cast (octets-to-alien (string-to-octets key)) (array unsigned-char))
         (length key)
         e)
      (std:clone-octets-from-alien 
       data
       (make-array i :element-type 'octet)))))

(defun %wbwi-write (db batch &optional opts)
  (with-errptr e (rocksdb-write-writebatch-wi (sap db) (sap (or opts (make-rdb-writeopts))) (sap batch) e)))
