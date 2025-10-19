;;; rdb/db.lisp --- RDB Database API

;; RocksDB Implementation of OBJ/DB protocol.

;;; Commentary:

;; The DB protocol is also partially implemented by the low-level structures
;; in rdb/obj.lisp.

;; It is safe to call most functions on the same underlying Alien RocksDB
;; object from multiple threads. Other objects such as WriteBatch and Iterator
;; /may/ require a Lisp-side synchronization.

;;;; Transactions:

;; RocksDB has several variations on the concept of 'transaction':

#| TransactionDB
When using a TransactionDB, all keys that are written are locked internally by
RocksDB to perform conflict detection. If a key cannot be locked, the
operation will return an error. When the transaction is committed, it is
guaranteed to succeed as long as the database is able to be written to.

A TransactionDB can be better for workloads with heavy concurrency compared to
an OptimisticTransactionDB. However, there is a small locking overhead when
TransactionDB is used. A TransactionDB will do conflict checking for all write
operations (Put, Delete and Merge), including writes performed outside a
Transaction.
|#

#| WriteBatch

The WriteBatch holds a sequence of edits to be made to the database - these
edits within the batch are applied in order when written.

Apart from its atomicity benefits, WriteBatch may also be used to speed up
bulk updates by placing lots of individual mutations into the same batch.

|#

#| WBWI

The WBWI (Write Batch With Index) encapsulates a WriteBatch and an Index into
that WriteBatch. The index in use is a Skip List. The purpose of the WBWI is
to sit above the DB, and offer the same basic operations as the DB,
i.e. Writes - Put, Delete, and Merge, and Reads - Get, and newIterator.

Write operations on the WBWI are serialized into the WriteBatch (of the WBWI)
rather than acting directly on the DB. The WriteBatch can later be written
atomically to the DB by calling db.write(wbwi).

Read operations can either be solely against the
WriteBatch (e.g. GetFromBatch), or they can be read-through operations. A
read-through operation, (e.g. GetFromBatchAndDB), first tries to read from the
WriteBatch, if there is no updated entry in the WriteBatch then it
subsequently reads from the DB.

The WBWI can be used as a component if one wishes to build Transaction
Semantics atop RocksDB. The WBWI by itself isolates the Write Path to a local
in-memory store and allows you to RYOW (Read-Your-Own-Writes) before data is
atomically written to the database.

It is a key component in RocksDB's Pessimistic and Optimistic Transaction
utility classes.  

|#

;; WBWIs are ideal as a transaction building block and should be used to build
;; higher-level transaction objects.

;;;; Snapshots:

;; Snapshots capture a point-in-time view of a RocksDB instance at the time of
;; creation. Snapshots do not persist across DB sessions and are internally
;; stored in a linked-list.

;;;; Checkpoints:

;; Checkpoints allow us to take a snapshot of a running RocksDB instance like
;; Snapshots, but in a separate directory. Checkpoints persist across DB
;; sessions. Checkpoints can be opened read-only or as read-write and be used
;; for both full and incremental backups (as long as backups are on the same
;; device).

;;;; Backups:

;; Backups are built on top of Checkpoints.

;; Backup Engines control a single directory that can store any number of
;; backups. It uses a custom on-disk format as shown below.

#| directory structure
/tmp/rocksdb_backup/
├── meta
│   └── 1
├── private
│   └── 1
│       ├── CURRENT
│       ├── MANIFEST-000008
|       └── OPTIONS-000009
└── shared_checksum
    └── 000007_1498774076_590.sst
|#

;;; Code:
(in-package :rdb)

;;; Backend
(defvar *rocksdb-backend-options* '(columns temp path (open . t) 
                                    destroy (close . t) 
                                    sap merge-op comparator prefix-op logger event-listener))

;; TODO 2024-12-31: may want to have a :STORE backend-option to allow a fresh
;; db to be backlined to a parent store instance.
(defvar *rdb-backend-options* (append *rocksdb-backend-options* '(backup secondary snapshots checkpoints)))

(defvar *rdb-default-column-name* "default")

(defmethods set-database-backend-option
  (((db rdb) (key (eql :close)) (val (eql :auto)))
   "Arrange for SHUTDOWN-DB to be called when there are no more references to DB."
   (sb-ext:finalize db (lambda () (shutdown-db db))))
  (((db rdb) (key (eql :merge-op)) val)
   "Assign a MERGE-OP to this database."
   (setf (db-opt db :merge-operator :push t) val))
  (((db rdb) (key (eql :comparator)) val)
   "Assign a custom COMPARATOR to this database."
   (setf (db-opt db :comparator :push t) val))
  (((db rdb) (key (eql :prefix-op)) val)
   "Assign a custom SLICETRANSFORM to this database to be used as a prefix
extractor."
   (setf (db-opt db :prefix-extractor :push t) val))
  (((db rdb) (key (eql :event-listener)) val)
   "Assign an EVENT-LISTENER to this database."
   (setf (db-opt db :event-listener :push t) val))
  (((db rdb) (key (eql :logger)) val)
   (setf (db-opt db :info-log :push t) val)))

(set-database-backend :rocksdb *rocksdb-backend-options*
                      (lambda () (load-rocksdb *save-database-backend-on-load*)))

(set-database-backend :rdb *rdb-backend-options*
                      (lambda () (db::%load-database-backend :rocksdb)))

(defmethod load-opts ((db rdb) &key backfill)
  (with-latest-options (name db) (db-opts cf-names cf-opts)
       (let ((cfs (coerce 
                   (loop for name across cf-names
                         for opt across cf-opts
                         collect 
                            (let ((cf-opts (make-rdb-opts)))
                              (setf (sap cf-opts) opt)
                              (when (eq backfill :full) (backfill-opts cf-opts :full t))
                              (make-rdb-cf name :opts cf-opts)))
                   'vector)))
         (setf (db-opts db) (make-rdb-opts* db-opts))
         (when backfill (backfill-opts (db-opts db) :full (eq backfill :full)))
         cfs)))

(defmethod make-db ((engine (eql :rocksdb)) &rest initargs &key 
                    name
                    merge-op
                    prefix-op
                    logger
                    event-listener
                    (opts (default-rdb-opts))
                    path)
  (declare (ignore engine initargs))
  (when merge-op
    (set-db-opt opts :merge-operator merge-op :push t))
  (when prefix-op
    (set-db-opt opts :prefix-extractor prefix-op :push t))
  (when logger
    (set-db-opt opts :info-log logger :push t))
  (when event-listener
    (set-db-opt opts :event-listener event-listener :push t))
  (let ((db (make-rdb 
             :name (or name (namestring path) (string-downcase (gensym "rocksdb"))) 
             :opts opts)))
    (push-opts db)
    db))

(defmethod query-db ((db rdb) (query (eql :get)) &key key column &allow-other-keys)
  (declare (ignore query))
  (get-val db key :column column))

;;; Column Families
(defclass rdb-column-family (rdb-column) 
  ((cf :initarg :cf :type rdb-cf :accessor cf))
  (:default-initargs :cf (make-rdb-cf (symbol-name (gensym "#"))))
  (:documentation "High-level Lisp-side RocksDB Column Family base class. Implements the COLUMN
protocol and contains a CF slot which contains an RDB-CF structure
object. (SAP CF) is the raw pointer."))

(defaccessor name ((self rdb-column-family)) (name (cf self)))
(defaccessor sap ((self rdb-column-family)) (sap (cf self)))
(defaccessor column-opts ((self rdb-column-family)) (rdb-cf-opts (cf self)))

(defun schema-from-rdb-column-families (columns)
  "Convert a sequence of RDB-COLUMN-FAMILYs to a SCHEMA."
  (apply 'make-schema 
	 (map 'list 
	      (lambda (x)
		(make-field :name (keywordicate (name x)) :type (column-type x)))
		columns)))

(defmethod destroy-column ((self rdb-column-family) &optional error)
  (destroy-column (cf self) error))

(defmethod close-column ((self rdb-column-family) &optional error)
  (close-column (cf self) error))

(defmethod load-field ((self rdb-column-family) (field field))
  (let ((type (field-type field))
        (ctype (column-type self)))
  (typecase type
    (null nil)
    (atom (if (atom ctype) 
              (setf ctype (cons ctype type))
              (setf (cdr ctype) type)))
    (list (setf (car ctype) (car type)
                (cdr ctype)
                (if (and (listp (cdr type))
                         (= 1 (length (cdr type))))
                    (cadr type)
                    (cdr type)))))
    self))

(defmethod change-class ((self field) (new-class (eql 'rdb-column-family)) &key)
  (make-instance new-class :cf (make-rdb-cf (field-name self)) :type (field-type self)))

(defmethod change-class ((self rdb-cf) (new-class (eql 'rdb-column-family)) &key)
  (make-instance new-class :cf self))

(defmethod change-class ((self column) (new-class (eql 'rdb-column-family)) &key name)
  (let ((ret (make-instance new-class :type (column-type self))))
    (when name (setf (name ret) name))
    ret))

;;; Database
(defclass rdb-database (database)
  ((txn :initform nil :type (or null rdb-optimistic-transaction-db) :initarg :txn :accessor transaction-db)
   (backup :initform nil :type (or null rdb-backup-engine) :initarg :backup :accessor db-backup)
   (snapshots :initform (make-array 0 :element-type 'rdb-snapshot :adjustable t)
              :type (vector rdb-snapshot)
              :initarg :snapshots 
              :accessor db-snapshots)
   (checkpoints :initform (make-array 0 :element-type 'rdb-checkpoint :adjustable t)
                :type (vector rdb-checkpoint)
                :initarg :checkpoints
                :accessor db-checkpoints)
   (secondary :initform nil :type (or null rdb-secondary-db) :initarg :secondary :accessor secondary-db)
   (columns :initarg :columns :accessor columns))
  (:default-initargs 
   :db (make-db :rocksdb :opts (default-rdb-opts))
   ;; Note that we don't pre-populate this slot with the 'default' column
   ;; which is present on creation of a RocksDB database. Usually there isn't
   ;; much need to access this column directly as you can just access the
   ;; database directly, which will access the default column internally.
   :columns (make-array 0 :element-type 'rdb-column-family
              :adjustable t
              :fill-pointer t)))

(defmethods set-database-backend-option
  (((db rdb-database) (key (eql :close)) (val (eql :auto)))
   "Arrange for SHUTDOWN-DB to be called when there are no more references to DB."
   (sb-ext:finalize db (lambda () (close-db db))))
  (((db rdb-database) (key (eql :merge-op)) val)
   "Assign a MERGE-OP to this database."
   (setf (db-opt db :merge-operator :push t) val))
  (((db rdb-database) (key (eql :comparator)) val)
   "Assign a custom COMPARATOR to this database."
   (setf (db-opt (db db) :comparator) val))
  (((db rdb-database) (key (eql :prefix-op)) val)
   "Assign a custom SLICETRANSFORM to this database to be used as a prefix
extractor."
   (setf (db-opt (db db) :prefix-extractor :push t) val))
  (((db rdb-database) (key (eql :event-listener)) val)
   "Assign an EVENT-LISTENER to this database."
   (setf (db-opt (db db) :event-listener :push t) val)))

(defmethod load-opts ((self rdb-database) &key (backfill t))
  ;; order is determined by RocksDB
  (setf (columns self)
        (map 'vector (lambda (x) (make-instance 'rdb-column-family :cf x))
             (load-opts (db self) :backfill backfill)))
  self)

(defmethod repair-db ((self rdb-database) &key)
  (repair-db (db self)))

(defmethod merge-columns ((self rdb-database) (columns vector))
  (loop for c across columns
        do (if-let ((found (find-column c self)))
             (setf (aref (columns self) (position found (columns self))) c)
             (vector-push-extend c (columns self)))))

(defmethod backfill-opts ((self rdb-database) &key (full t))
  (backfill-opts (db-opts self) :full full))

(defmethod reset ((self rdb-database) &key (columns t) (opts t))
  (when columns 
    (close-columns self) 
    (setf (columns self)
          (make-array 0 :element-type 'rdb-column-family
                        :adjustable t
                        :fill-pointer t)))
  (when opts
    (setf (db-opts self) (if (eql t opts) (default-rdb-opts) opts))))

(defmethod open-column ((self rdb-database) (col string) &key)
  (open-column (db self) (cf (find-column col self))))

(defmethod open-column ((self rdb-database) (col symbol) &key)
  (open-column (db self) (cf (find-column (string-downcase col) self))))

(defmethod open-column ((self rdb-database) (col rdb-column-family) &key)
  (open-column (db self) (cf col)))

(defmethod open-columns ((self rdb-database) &rest columns)
  (dolist (c columns)
    (open-column self c)))

(defmethod find-column ((cf string) (self rdb-database) &key)
  (find cf (columns self) :key 'name :test 'equal))

(defmethod add-column ((cf rdb-cf) (db rdb-database))
  (vector-push-extend (make-instance 'rdb-column-family :cf cf) (columns db)))

(defmethod open-with-columns ((db rdb-database) &rest names)
  (let ((cols 
          (coerce
           (if (null names)
               (columns db)
               (loop for n in names
                     collect (if-let ((col (find-column n db)))
                               col
                               (add-column 
                                (make-instance 'rdb-column-family 
                                  :cf (make-rdb-cf n)) 
                                db))))
           'vector)))
    (multiple-value-bind (db-sap cfs) (open-cfs-raw (db-opts db) (name db)
                                                    (loop for c across cols
                                                          collect (name c))
                                                    (loop for c across cols
                                                          collect (sap (column-opts c))))
      (setf (sap db) db-sap)
      (loop for c across cfs
            do (when-let ((col (find-column (name c) db)))
                 (setf (sap (cf col)) c)))
      db)))

(defmethod open-columns* ((self rdb-database))
  (let ((names) (opts))
    (loop for c across (columns self)
          do (push (name c) names)
          do (push (sap (column-opts c)) opts))
    (nreversef names)
    (nreversef opts)
    (unless (member *rdb-default-column-name* names :test 'string=)
      (push *rdb-default-column-name* names)
      (push (sap (db-opts self)) opts))
    (multiple-value-bind (db cfs)
        (open-cfs-raw (sap (db-opts self)) (name self) names opts)
      (setf (sap self) db)
      (let ((len (length names)))
        (loop for n in names
              for i below len
              for cf = (deref cfs i)
              do (when-let ((c (find-column (pop names) self)))
                   (setf (sap c) cf)))
        self))))

(defmethod close-columns ((self rdb-database))
  (loop for cf across (columns self)
        ;; unless (string= (name cf) *rdb-default-column-name*)
        do (close-column cf)))

(defmethods insert-key 
  (((self rdb-database) key val &key column)
   (if-let ((column (and column (find-column column self))))
     (if-let ((sap (sap column)))
       (put-cf-raw
        (sap self)
        sap
        key
        val
        (rocksdb-writeoptions-create))
       (rdb-error "column-family is not open"))
     (put-key self key val)))
  (((self rdb-database) (key string) (val string) &key column)
   (if-let ((column (and column (find-column column self))))
     (if-let ((sap (sap column)))
       (put-cf-raw
        (sap self)
        sap
        (string-to-octets key)
        (string-to-octets val)
        (rocksdb-writeoptions-create))
       (rdb-error "column-family is not open"))
     (put-key self key val)))
  (((self rdb-database) (key string) val &key column)
   (if-let ((column (and column (find-column column self))))
     (if-let ((sap (sap column)))
       (put-cf-raw
        (sap self)
        sap
        (string-to-octets key)
        val
        (rocksdb-writeoptions-create))
       (rdb-error "column-family is not open"))
     (put-key self key val)))
  (((self rdb-database) key (val string) &key column)
   (if-let ((column (and column (find-column column self))))
     (if-let ((sap (sap column)))
       (put-cf-raw
        (sap self)
        sap
        key
        (string-to-octets val)
        (rocksdb-writeoptions-create))
       (rdb-error "column-family is not open"))
     (put-key self key val)))
  (((self rdb) (key string) (val string) &key column)
   (insert-key self (string-to-octets key) (string-to-octets val) :column column))
  (((self rdb) (key string) val &key column)
   (insert-key self (string-to-octets key) val :column column))
  (((self rdb) key (val string) &key column)
   (insert-key self key (string-to-octets val) :column column)))

(defmethod insert-kv ((self rdb) (kv kv) &key column (opts (rocksdb-writeoptions-create)))
  (if column
      (let ((column (etypecase column
                  (rdb-cf column)
                  (t (find column (columns self)
                           :key 'name
                           :test 'equal)))))
        (put-cf-raw (sap self)
                    (sap column)
                    (kv-key kv)
                    (kv-val kv)
                    opts))
      (put-kv self kv)))

(defmethod iter ((self rdb-database) &key column (opts (rocksdb-readoptions-create)))
  (typecase column
    (rdb-column-family (iter (db self) :cf (cf column) :opts opts))
    (null (iter (db self) :opts opts))
    (symbol (iter (db self) :cf (cf (find-column column self)) :opts opts))
    (simple-string (iter (db self) :cf (cf (find-column column self)) :opts opts))
    (rdb-cf (iter (db self) :cf column :opts opts))
    (t (iter (db self) :opts opts :cf column))))

(defmethods get-val 
  (((self rdb-database) (key string) &key (opts (rocksdb-readoptions-create)) column)
   (let ((sap (sap self)))
     (if column
         (get-cf-str-raw sap (sap (find-column column self)) key opts)
         (get-kv-str-raw sap key opts))))
  (((self rdb-database) key &key (opts (rocksdb-readoptions-create)) column)
   (let ((sap (sap self)))
     (if column
         (get-cf-raw sap (sap (find-column column self)) key opts)
         (get-kv-raw sap key opts)))))

(defmethod multi-get ((self rdb-database) keys &key (data-type 'octet-vector) (opts (rocksdb-readoptions-create)) columns)
  (multi-get (db self) keys :data-type data-type :opts opts :cf (mapcar 'cf columns)))

(defmethod create-column ((db rdb-database) (col rdb-column-family))
  (if (equal (name col) *rdb-default-column-name*)
      (rdb-default-column-warning "ignoring attempt to create 'default' column-family: ~A" col)
      (setf (sap col) (create-cf-raw (sap db) (name col) (sap (column-opts col)))))
  ;; (open-column db col)
  col)

(defmethod create-columns ((self rdb-database))
  (if (null (sap self))
      (warn 'db-missing :message "ignoring attempt to create column-families before opening")
      (loop for cf across (columns self)
            do (create-column self cf))))

(defmethod find-column ((cf string) (self rdb-database) &key)
  "Find a column by name."
  (find cf (columns self) :key 'name :test 'equal))

(defmethod find-column ((cf symbol) (self rdb-database) &key)
  (find (string-downcase cf) (columns self) :key 'name :test 'string=))

(defmethod find-column ((col rdb-column-family) (self rdb-database) &key)
  (find (string-downcase (name col)) (columns self) :key 'name :test 'string=))

(defmethod (setf find-column) ((new rdb-column-family) (cf string) (self rdb-database) &key)
  "Find and replace a column by name."
  (nsubstitute new (find-column cf self) (columns self)))

(defmethod database-version ((self rdb-database))
  "Return the version tag or nil if unmarked"
  (when-let ((db (and #1=(db self) (sap #1#))))
    (rocksdb-property-value db "rocksdb.current-super-version-number")))

(defaccessor name ((self rdb-database)) (name (db self)))
(defaccessor sap ((self rdb-database)) (sap (db self)))
(defaccessor db-opts ((self rdb-database)) (db-opts (db self)))
(defaccessor* db-opt 
    ((self rdb-database) key) (db-opt (db-opts self) key)
    (new (self rdb-database) key &key push)
  (prog1 (setf (db-opt (db-opts self) key) new)
    (when push (push-sap (db-opts self) key))))

(defmethods db-prop 
  (((self rdb-database) (name string))
   (db-prop (db self) name))
  (((self rdb-database) (name symbol))
   (db-prop (db self) (string-downcase (concatenate 'string "rocksdb." (symbol-name name))))))

(defmethod push-opts ((self rdb-database))
  (with-slots (opts) (db self)
    (push-sap* opts)))

(defmethod print-stats ((self rdb-database) &optional stream)
  (print-stats (db self) stream))

(defmethod db-metadata ((self rdb-database) &optional type)
  (db-metadata (db self) type))

(defmethod db-stats ((self rdb-database) &optional (type (rocksdb-statistics-level "all")))
  (db-stats (db self) type))

(defmethod ingest-db ((self rdb-database) files &key (opts (rocksdb-ingestexternalfileoptions-create))
                                                     column)
  (if column
      (ingest-db (db self) files :opts opts :column (find-column column self))
      (ingest-db (db self) files :opts opts)))

(defmethods make-db 
  (((engine (eql :rdb)) &rest initargs &key columns &allow-other-keys)
   (declare (ignore engine))
   (remf initargs :columns)
   (let ((db (make-instance 'rdb-database :db (apply 'make-db :rocksdb initargs))))
     (when columns (setf (columns db) (coerce (mapcar (lambda (x) (cf x)) columns) 'vector)))
     db))
  (((engine (eql :rdb-backup)) &key path (db *db*))
   (setf (db-backup db) (backup-db db :path path)))
  (((engine (eql :rdb-transaction)) &key path opts (db *db*))
   (setf (transaction-db db) (open-transaction-db db :opts opts :path path)))
  (((engine (eql :rdb-secondary)) &key path opts (db *db*))
   (setf (secondary-db db) (open-secondary-db db :opts opts :path path))))

(defmethod derive-schema ((self rdb-database))
  (apply 'make-schema
         (loop for c across (columns self)
               collect (cf-to-field (cf c)))))

(defmethod open-db ((self rdb-database)) (open-db (db self)) self)

(defmethod open-transaction-db ((self rdb-database) &key path (opts (rocksdb-transactiondb-options-create)) optimistic)
  (setf (transaction-db self) (open-transaction-db (db self) :opts opts :path path :optimistic optimistic)))

(defmethod open-backup-engine ((self rdb-database) &key path) 
  (setf (db-backup self) (open-backup-engine (db self) :path path)))

(defmethod open-secondary-db ((self rdb-database) &key path opts) 
  (setf (secondary-db self) (open-secondary-db (db self) :opts opts :path path)))

(defmethod open-checkpoint-db ((self rdb-database) &key path)
  (vector-push-extend (%make-checkpoint (sap self) path) (db-checkpoints self)))

(defmethod snapshot-db ((self rdb-database))
  (vector-push-extend (snapshot-db (db self)) (db-snapshots self)))

(defmethod flush-db ((self rdb-database) &rest args &key &allow-other-keys) (apply 'flush-db (db self) args))

(defmethod close-db ((self rdb-database) &key) 
  (close-columns self)
  (close-db (db self)))

(defmethod db-closed-p ((self rdb-database)) (db-closed-p (db self)))
(defmethod db-open-p ((self rdb-database)) (db-open-p (db self)))

(defmethod destroy-db ((self rdb-database))
  (destroy-db (db self)))

(defmethod close-backup-engine ((self rdb-database))
  (with-slots (backup) self
    (unless (null backup)
      (setf backup (close-backup-engine backup)))))

(defmethod close-transaction-db ((self rdb-database))
  (when-let ((sap (transaction-db self)))
    (close-transaction-db sap)))

(defmethod shutdown-db ((self rdb-database) &key) 
  (close-backup-engine self)
  (close-transaction-db self)
  (close-columns self)
  (shutdown-db (db self)))

(defmethod get-value (elt (self rdb-database))
  (get-value elt (db self)))

(defmethod put-key ((self rdb-database) key val)
  (put-key (db self) key val))

(defmethod put-kv ((self rdb-database) (kv kv))
  (put-kv (db self) kv))

(defmethod delete-key ((self rdb-database) key &key)
  (delete-key (db self) key))

(defmethod merge-key ((self rdb-database) key val &key (opts (rocksdb-writeoptions-create)))
  (merge-key (db self) key val :opts opts))

(defmethod merge-kv ((self rdb-database) kv &key (opts (rocksdb-writeoptions-create)))
  (merge-kv-raw (sap self) (kv-key kv) (kv-val kv) opts))

(defmethod add-column (col (self rdb-database))
  (vector-push-extend col (coerce (columns self) 'vector)))

(defmethod destroy-columns ((self rdb-database))
  (with-slots (columns) self
    (loop for cf across columns
          do (setf cf (destroy-column cf)))))

(defmethod load-schema ((self rdb-database) (schema schema))
  "Load SCHEMA into rdb database object SELF. This will add any missing rdb-cfs
and update existing key/value types for cfs with the same name. Existing cfs
only get their type slots updated on non-nil values."
  (loop for field across (fields schema)
        do (if-let ((col (find-column (field-name field) self)))
             (load-field col field)
             (add-column
              (load-field
               (make-instance 'rdb-column-family :cf (make-rdb-cf (field-name field)) :type (field-type field))
               field)
              self))
        finally (return self)))

;;; Transactions
(defmethod make-transaction ((self rdb-database)
                             &key (write-opts (rocksdb-writeoptions-create))
                                  path
                                  (name (name self))
                                  txn
                                  optimistic
                                  (opts (rocksdb-transaction-options-create))
                                  (db-opts (rocksdb-transactiondb-options-create)))
  (with-errptr e
    (let ((txn-db (or (transaction-db self)
                      (setf (transaction-db self)
                            (open-transaction-db self :opts db-opts :path path :optimistic optimistic)))))
      (let ((obj (make-transaction txn-db :write-opts write-opts 
                                          :opts opts 
                                          :txn txn)))
        (when name (setf (name obj) name))
        obj))))

(defmethod execute-transaction ((self rdb-database) (fn function) &key (txn *txn*))
  (funcall fn)
  (when txn
    (commit-transaction txn)
    (rocksdb-transaction-destroy txn)))

;;; Collections
(defclass rdb-collection (database-collection)
  ((collection :initform (coerce nil db::*database-collection-type*))))
