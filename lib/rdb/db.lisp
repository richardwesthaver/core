;;; rdb/db.lisp --- RDB Database API

;; RocksDB Implementation of OBJ/DB protocol.

;;; Code:
(in-package :rdb)

;;; Backend
(defvar *rocksdb-backend-options* 
  '(columns temp path (open . t)
    destroy (close . t) 
    sap merge-op comparator prefix-op logger event-listener))

;; TODO 2024-12-31: may want to have a :STORE backend-option to allow a fresh
;; db to be backlined to a parent store instance.
(defvar *rdb-backend-options* (append *rocksdb-backend-options* '(backup secondary snapshots checkpoints)))

(defvar *rdb-default-column-name* "default")

(set-database-backend :rocksdb *rocksdb-backend-options*
                      (lambda () (load-rocksdb *save-database-backend-on-load*)))

(set-database-backend :rdb *rdb-backend-options*
                      (lambda () (db::%load-database-backend :rocksdb)))

(defmethod make-db ((engine (eql :rocksdb)) 
                    &key
                    merge-op
                    prefix-op
                    logger
                    event-listener
                    (opts (default-rocksdb-options))
                    open
                    path)
  (declare (ignore engine))
  (when merge-op (rocksdb-options-set-merge-operator opts merge-op))
  (when prefix-op (rocksdb-options-set-prefix-extractor opts prefix-op))
  (when logger (rocksdb-options-set-info-log opts logger))
  (when event-listener (rocksdb-options-add-eventlistener opts event-listener))
  (if open
      (%open-db path opts) ; open the db, OR
      (cons path opts))) ; return a cons


;;; Database
(defclass rdb (database)
  ((options :initform (default-rocksdb-options) :accessor options))
  (:documentation "Standard RocksDB database wrapper.
OPTIONS is an alien ROCKSDB-OPTIONS pointer."))

(defmethod load-opts ((db rdb) &key)
  (with-latest-options (name db) (db-opts cf-names cf-opts)
       (let ((cfs (loop for name across cf-names
                        for opt across cf-opts
                        collect (make-instance 'column-family :name name :options opt))))
         (setf (options db) db-opts)
         (setf (columns db) cfs)
         db)))

(defmethods set-database-backend-option
  (((db rdb) (key (eql :close)) (val (eql :auto)))
   "Arrange for SHUTDOWN-DB to be called when there are no more references to DB."
   (sb-ext:finalize db (lambda () (shutdown-db db))))
  (((db rdb) (key (eql :merge-op)) val)
   "Assign a MERGE-OP to this database."
   (setf (opt db :merge-operator) val))
  (((db rdb) (key (eql :comparator)) val)
   "Assign a custom COMPARATOR to this database."
   (setf (opt db :comparator) val))
  (((db rdb) (key (eql :prefix-op)) val)
   "Assign a custom SLICETRANSFORM to this database to be used as a prefix
extractor."
   (setf (opt db :prefix-extractor) val))
  (((db rdb) (key (eql :event-listener)) val)
   "Assign an EVENT-LISTENER to this database."
   (setf (opt db :event-listener) val))
  (((db rdb) (key (eql :logger)) val)
   (setf (opt db :info-log) val)))

(defclass column-family (rdb)
  ((name :initform "default" :initarg :name :accessor name))
  (:documentation "RocksDB Column Family.
Inherits directly from the RDB class. The DB slot is a
ROCKSDB-COLUMN-FAMILY-HANDLE."))
(defclass trdb (rdb)
  ((transaction-options :initform (rocksdb-transaction-options-create) :accessor transaction-options))
  (:documentation "Transaction DB.
TRANSACTION-OPTIONS is an alien ROCKSDB-TRANSACTIONDB-OPTIONS pointer."))
(defclass otrdb (rdb) ()
  (:documentation "Optimistic Transaction DB."))
(defclass simple-rdb (rdb)
  ((backup :initform nil :type (or null (alien (* rocksdb-backup-engine))) :initarg :backup :accessor db-backup)
   (snapshots :initform nil
              :initarg :snapshots 
              :accessor db-snapshots)
   (checkpoints :initform nil
                :initarg :checkpoints
                :accessor db-checkpoints)
   (secondary :initform nil :type (or null (alien (* rocksdb))) :initarg :secondary :accessor secondary-db)
   (columns :initarg :columns :accessor columns))
  (:default-initargs 
   ;; Note that we don't pre-populate this slot with the 'default' column
   ;; which is present on creation of a RocksDB database. Usually there isn't
   ;; much need to access this column directly as you can just access the
   ;; database directly, which will access the default column internally.
   :columns nil))

(defclass simple-column-family (column-family rdb-column) ()
  (:default-initargs :name (symbol-name (gensym "#")))
  (:documentation "COLUMN support for RocksDB Column Families."))

;; HACK 2026-08-06: 
;; (defun rdb (path &key))
(defmethods set-database-backend-option
  (((db rdb) (key (eql :close)) (val (eql :auto)))
   "Arrange for SHUTDOWN-DB to be called when there are no more references to DB."
   (sb-ext:finalize db (lambda () (close-db db))))
  (((db rdb) (key (eql :merge-op)) val)
   "Assign a MERGE-OP to this database."
   (setf (opt db :merge-operator) val))
  (((db rdb) (key (eql :comparator)) val)
   "Assign a custom COMPARATOR to this database."
   (setf (opt db :comparator) val))
  (((db rdb) (key (eql :prefix-op)) val)
   "Assign a custom SLICETRANSFORM to this database to be used as a prefix
extractor."
   (setf (opt db :prefix-extractor) val))
  (((db rdb) (key (eql :event-listener)) val)
   "Assign an EVENT-LISTENER to this database."
   (setf (opt db :event-listener) val)))

(defmethod repair-db ((self rdb) &key)
  (%repair-db (path self)))

(defmethod load-opts ((self rdb) &key (backfill t))
  ;; order is determined by RocksDB
  (setf (columns self)
        (lambda (x) (make-instance 'column-family :cf x)
          (load-opts (db self) :backfill backfill)))
  self)

(defmethod merge-columns ((self rdb) (columns list))
  ;; TODO 2026-08-07: using lists now, use list MERGE
  (loop for c in columns
        do (if-let ((found (find-column c self)))
             (setf (nth (columns self) (position found (columns self))) c)
             (push c (columns self)))))

(defmethod reset ((self rdb) &key (columns t) (opts (default-rocksdb-options)))
  (when columns 
    (close-columns self) 
    (setf (columns self)
          (make-array 0 :element-type 'column-family
                        :adjustable t
                        :fill-pointer t)))
  (setf (options self) opts)
  self)

(defmethod open-column ((self rdb) col &key)
  (open-column self (db (find-column col self))))

(defmethod open-column ((self rdb) (col column-family) &key)
  (ifret (db col)
    (setf (db col) (create-column self col))))

(defmethod open-columns ((self rdb) &rest columns)
  (dolist (c columns)
    (open-column self c)))

(defmethod find-column ((cf string) (self rdb) &key)
  (find cf (columns self) :key 'name :test 'equal))

(defmethod add-column ((cf t) (db rdb))
  (push (make-instance 'column-family :db cf) (columns db)))

(defmethod open-with-columns ((db rdb) &rest names)
  (let ((cols (if (null names)
                  (columns db)
                  (loop for n in names
                        collect (if-let ((col (find-column n db)))
                                  col
                                  (add-column 
                                   (make-instance 'column-family 
                                     :db (%create-cf (db db) n))
                                   db))))))
    (multiple-value-bind (db-sap cfs) (%open-cfs (opts db) (name db)
                                                    (loop for c across cols
                                                          collect (name c))
                                                    (loop for c across cols
                                                          collect (options c)))
      (setf (sap db) db-sap)
      (loop for c across cfs
            do (when-let ((col (find-column (name c) db)))
                 (setf (db col) c)))
      db)))

(defmethod open-columns* ((self rdb))
  (let ((names) (opts))
    (loop for c across (columns self)
          do (push (name c) names)
          do (push (options c) opts))
    (nreversef names)
    (nreversef opts)
    (unless (member *rdb-default-column-name* names :test 'string=)
      (push *rdb-default-column-name* names)
      (push (opts self) opts))
    (multiple-value-bind (db cfs)
        (%open-cfs (opts self) (name self) names opts)
      (setf (db self) db)
      (let ((len (length names)))
        (loop for n in names
              for i below len
              for cf = (deref cfs i)
              do (when-let ((c (find-column (pop names) self)))
                   (setf (db c) cf)))
        self))))

(defmethod close-columns ((self rdb))
  (loop for cf across (columns self)
        ;; unless (string= (name cf) *rdb-default-column-name*)
        do (close-db cf)))

(defmethods insert-key 
  (((self simple-rdb) key val &key column)
   (if-let ((column (and column (find-column column self))))
     (if-let ((sap (sap column)))
       (%put-cf
        (sap self)
        sap
        key
        val
        (rocksdb-writeoptions-create))
       (simple-rdb-error "column-family is not open"))
     (put-key self key val)))
  (((self simple-rdb) (key string) (val string) &key column)
   (if-let ((column (and column (find-column column self))))
     (if-let ((sap (sap column)))
       (%put-cf
        (sap self)
        sap
        (string-to-octets key)
        (string-to-octets val)
        (rocksdb-writeoptions-create))
       (simple-rdb-error "column-family is not open"))
     (put-key self key val)))
  (((self simple-rdb) (key string) val &key column)
   (if-let ((column (and column (find-column column self))))
     (if-let ((sap (sap column)))
       (%put-cf
        (sap self)
        sap
        (string-to-octets key)
        val
        (rocksdb-writeoptions-create))
       (simple-rdb-error "column-family is not open"))
     (put-key self key val)))
  (((self simple-rdb) key (val string) &key column)
   (if-let ((column (and column (find-column column self))))
     (if-let ((sap (sap column)))
       (%put-cf
        (sap self)
        sap
        key
        (string-to-octets val)
        (rocksdb-writeoptions-create))
       (simple-rdb-error "column-family is not open"))
     (put-key self key val)))
  (((self rdb) (key string) (val string) &key column)
   (insert-key self (string-to-octets key) (string-to-octets val) :column column))
  (((self rdb) (key string) val &key column)
   (insert-key self (string-to-octets key) val :column column))
  (((self rdb) key (val string) &key column)
   (insert-key self key (string-to-octets val) :column column)))

(defmethod iter ((self rdb) &key column columns (opts (rocksdb-readoptions-create)))
  (typecase column
    (column-family (rocksdb-create-iterator-cf (db self) opts (db column)))
    (null (if columns
              (%create-iterators (db self) (options self) (mapcar 'db columns))
              (rocksdb-create-iterator (db self) opts)))
    (symbol (rocksdb-create-iterator-cf (db self) opts (db (find-column column self))))
    (simple-string (rocksdb-create-iterator-cf (db self) opts (db (find-column column self))))))

(defmethod iter ((self trdb) &key column (opts (rocksdb-readoptions-create)))
  (typecase column
    (column-family (rocksdb-transactiondb-create-iterator-cf (db self) opts (db column)))
    (null (rocksdb-transactiondb-create-iterator (db self) opts))
    (symbol (rocksdb-transactiondb-create-iterator-cf (db self) opts (db (find-column column self))))
    (simple-string (rocksdb-transactiondb-create-iterator-cf (db self) opts (db (find-column column self))))))

(defmethods get-val 
  (((self rdb) (key string) &key (opts (rocksdb-readoptions-create)) column pin)
   (unless-null-db () self
     (if column
         (%get-cf-str db (db (find-column column self)) key opts pin)
         (%get-kv-str db key opts pin))))
  (((self rdb) key &key (opts (rocksdb-readoptions-create)) column pin)
   (unless-null-db () self
     (if column
         (%get-cf db (db (find-column column self)) key opts pin)
         (%get-kv db key opts pin))))
  (((self trdb) key &key (opts (rocksdb-readoptions-create)) column pin)
   (unless-null-db () self
     (if column
         (%transactiondb-get-cf db (db (find-column column self)) key opts pin)
         (%transactiondb-get-kv db key opts pin))))
  (((self trdb) (key string) &key (opts (rocksdb-readoptions-create)) column pin)
   (unless-null-db () self
     (if column
         (%transactiondb-get-cf-str db (db (find-column column self)) key opts pin)
         (%transactiondb-get-kv-str db key opts pin)))))

(defmethod multi-get ((self rdb) keys &key (opts (rocksdb-readoptions-create)) columns)
  (if columns
      (%multi-get-cf-kv (db self) (mapcar 'db columns) keys opts)
      (%multi-get-kv (db self) keys opts)))

(defmethod multi-get ((self simple-rdb) keys &key (data-type 'octet-vector) (opts (rocksdb-readoptions-create)) cf)
  (if cf
      (ecase data-type
        (octet-vector (%multi-get-cf-kv (sap self) keys opts (sap cf)))
        (string (%multi-get-cf-kv-str (sap self) keys opts (sap cf))))
      (ecase data-type
        (octet-vector (%multi-get-kv (sap self) keys opts))
        (string (%multi-get-kv-str (sap self) keys opts)))))

(defmethod create-column ((db rdb) (col column-family))
  (if (equal (name col) *rdb-default-column-name*)
      (rdb-default-column-warning "ignoring attempt to create 'default' column-family: ~A" col)
      (setf (db col) (%create-cf (db db) (name col) (options col))))
  ;; (open-column db col)
  col)

(defmethod create-columns ((self rdb))
  (if (null (db self))
      (warn 'db-missing :message "ignoring attempt to create column-families before opening")
      (loop for cf across (columns self)
            do (create-column self cf))))

(defmethod find-column ((cf string) (self rdb) &key)
  "Find a column by name."
  (find cf (columns self) :key 'name :test 'equal))

(defmethod find-column ((cf symbol) (self rdb) &key)
  (find (string-downcase cf) (columns self) :key 'name :test 'string=))

(defmethod find-column ((col column-family) (self rdb) &key)
  (find (string-downcase (name col)) (columns self) :key 'name :test 'string=))

(defmethod (setf find-column) ((new column-family) (cf string) (self rdb) &key)
  "Find and replace a column by name."
  (nsubstitute new (find-column cf self) (columns self)))

(defmethod database-version ((self rdb))
  "Return the version tag or nil if unmarked"
  (prop self "rocksdb.current-super-version-number"))

(defaccessor name ((self rdb)) (path self))
(defaccessor sap ((self rdb)) (db self))
(defaccessor opts ((self rdb) &key) (options self))
;; TODO
(defaccessor opt ((self rdb) key) (opt (opts self) key))
(defmethods prop 
  (((self rdb) (name string))
   (unless-null-db () self
     (rocksdb-property-value db name)))
  (((self rdb) (name symbol))
   (prop self (string-downcase (concatenate 'string "rocksdb." (symbol-name name))))))

(defmethod ingest-db ((self rdb) (files list) &key column (opts (rocksdb-ingestexternalfileoptions-create)))
  (if column
      (%ingest-db-cf (db self) (db column) files opts)
      (%ingest-db (db self) files opts)))

(defmethods make-db 
  (((engine (eql :rdb)) &rest initargs &key columns &allow-other-keys)
   (declare (ignore engine))
   ;; HACK 2026-08-07: 
   (remf initargs :columns)
   (make-instance 'simple-rdb :db (cdr (apply 'make-db :rocksdb initargs)) :columns columns))
  (((engine (eql :rdb-backup)) &key path (db *db*))
   (setf (db-backup db) (backup-db db :path path)))
  (((engine (eql :rdb-transaction)) &rest initargs &key columns &allow-other-keys)
   (remf initargs :columns)
   (let ((db (make-instance 'trdb :db (apply 'make-db :rocksdb-transaction initargs))))
     (when columns (setf (columns db) (mapcar 'db columns)))
     db))
  (((engine (eql :rdb-secondary)) &key path opts (db *db*))
   (setf (secondary-db db) (open-secondary-db db :opts opts :path path))))

(defmethod derive-schema ((self rdb))
  (apply 'make-schema
         (loop for c across (columns self)
               collect (field-from-cf (db c)))))

(defmethod open-db ((self rdb))
  (with-slots (path db options) self
    (if db
        (progn
          (cerror "Ignore and continue" 'open-db-error 
                  :db db
                  :message "Database is already open")
          db)
        (setf db (%open-db path options)))))

(defmethod open-db ((self trdb))
  (with-slots (path db options transaction-options) self
    (if db
        (progn
          (cerror "Ignore and continue" 'open-db-error
                  :db db
                  :message "Database is already open")
          db)
        (setf db (%open-transactiondb options transaction-options path)))))

(defmethod open-db ((self otrdb))
  (with-slots (path db options) self
    (if db
        (progn
          (cerror "Ignore and continue" 'open-db-error
                  :db db
                  :message "Database is already open")
          db)
        (setf db (%open-optimistictransactiondb options path)))))

(defmethod backup-db ((self rdb) &key path (opts (default-rocksdb-backup-engine-options)))
  (%open-backup-engine path opts))

(defmethod open-secondary-db ((self rdb) &key path opts) 
  (setf (secondary-db self) (%open-db-secondary opts (path self) path)))

(defmethod close-secondary-db ((self rdb))
  (with-slots (secondary-db) self
    (unless (null secondary-db)
      (setf (secondary-db self) (%close-db secondary-db)))))

(defmethod checkpoint ((self rdb) &key path log-size-for-flush)
  (unless-null-db () self
    (let ((chk (%make-checkpoint db)))
      (%create-checkpoint chk path log-size-for-flush))))

(defmethod checkpoint :around ((self simple-rdb) &rest args)
  (when-let ((chk (apply 'call-next-method args)))
    (push chk (db-checkpoints self))))

(defmethod snapshot-db ((self rdb))
  (unless-null-db () self
    (%create-snapshot db)))

(defmethod snapshot-db :around ((self simple-rdb))
   (push
    (call-next-method self)
    (db-snapshots self)))

(defmethod restore-db ((self rdb) (from string) &key id opts)
  (unless-null-db (path) self
    (%restore-from-backup (backup-db self :path from) path from id opts)))

(defmethod backup ((self rdb) &key path)
  (unless-null-db (options) self
    (if (null path)
        (error 'open-backup-engine-error :db db
                                         :message "PATH must not be nil when no backups exist")
        (%create-new-backup (open-backup-engine self :path path) db))))
(defmethod backup :around ((self simple-rdb) &rest args)
  (setf (db-backup self) (apply 'call-next-method args)))
(defmethod flush-db ((self rdb) &key wait)
  (%flush-db (db self) wait))
(defmethod close-db :before ((self rdb) &key)
  (close-columns self))
(defmethod close-db ((self rdb) &key) 
  (rocksdb-close (db self)))
(defmethod close-db ((self trdb) &key)
  (rocksdb-transactiondb-close (db self))
  (when-let ((topt (transaction-options self)))
    (rocksdb-transactiondb-options-destroy topt)))
(defmethod close-db ((self otrdb) &key)
  (rocksdb-optimistictransactiondb-close (db self)))
(defmethod close-db :after ((self rdb) &key)
  (when-let ((opt (options self)))
    (rocksdb-options-destroy opt)))
(defmethod print-object ((self rdb) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":open ~A" (db-open-p self))))

(defmethod db-open-p ((self rdb))
  (with-slots (db) self
    (and db (typep db 'alien) (not (null db)))))

(defmethod db-closed-p ((self rdb))
  (consp (db self)))

(defmethod destroy-db ((self rdb))
  ;; close all handles before destruction ensues
  (close-db self)
  (%destroy-db (namestring (path self))))

(defmethod close-backup ((self rdb))
  (with-slots (backup) self
    (unless (null backup)
      (setf backup (%close-backup-engine backup)))))

(defmethod shutdown-db ((self rdb) &key wait)
  (log:trace! "shutting down database" (path self))
  (close-backup self)
  (close-columns self)
  (when-let ((db (db self)))
    (rocksdb-cancel-all-background-work db wait)
    (close-db self)))

(defmethod get-value (elt (self rdb))
  (%get-kv (db self) elt *default-rocksdb-readoptions*))

(defmethod get-value (elt (self trdb))
  (%transactiondb-get-kv (db self) elt *default-rocksdb-readoptions*))

(defmethods put-key 
  (((self rdb) (key t) (val t))
   (%put-kv
    (db self)
    key
    val))
  (((self rdb) (key string) (val string))
   (%put-kv
    (db self)
    (sb-ext:string-to-octets key)
    (sb-ext:string-to-octets val))))

(defmethod delete-key ((self rdb) key &key (opts (default-rocksdb-writeoptions)))
  (%delete-kv (db self) key opts))

(defmethod merge-key ((self rdb) key val &key (opts (rocksdb-writeoptions-create)) column)
  (if column
      (%merge-cf (db self) (find-column column self) key val opts)
      (%merge-kv (db self) key val opts)))

(defmethod merge-key ((self rdb) (key string) (val string) &key (opts (rocksdb-writeoptions-create)) column)
  (if column
      (%merge-cf-str (db self) (find-column column self) key val opts)
      (%merge-kv-str (db self) key val opts)))

(defmethod add-column (col (self simple-rdb))
  (push col (columns self)))

(defmethod close-columns ((self rdb))
  (with-slots (columns) self
    (loop for cf across columns
          do (setf cf (close-db cf)))))

(defmethod load-schema ((self rdb) (schema schema))
  "Load SCHEMA into rdb database object SELF. This will add any missing CFs
and update existing key/value types for cfs with the same name. Existing CFs
only get their type slots updated on non-nil values."
  (loop for field across (fields schema)
        do (if-let ((col (find-column (name field) self)))
             (load-field col field)
             (add-column
              (load-field
               (make-instance 'simple-column-family 
                 :db (unless-null-db () self
                       (%create-cf db (name field)))
                 :type (field-type field))
               field)
              self))
        finally (return self)))

;;; Column Families
(defmethod name ((self column-family)) (%cf-name (db self)))
(defaccessor sap ((self column-family)) (db self))
(defmethod id ((self column-family)) (%cf-id (db self)))

(defun schema-from-simple-column-families (columns)
  "Convert a sequence of SIMPLE-COLUMN-FAMILYs to a SCHEMA."
  (apply 'make-schema 
     (map 'list 
          (lambda (x)
        (make-field :name (keywordicate (name x)) :type (column-type x)))
        columns)))

(defmethod free ((self column-family))
  (setf (db self) (%destroy-cf (db self))))

(defmethod close-db ((self column-family) &key)
  (unless (null (db self))
    (free self)))

(defmethod load-field ((self simple-column-family) (field field))
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

(defmethod change-class ((self field) (new-class (eql 'simple-column-family)) &key)
  (make-instance new-class :name (name self) :type (field-type self)))

(defmethod change-class ((self system-area-pointer) (new-class (eql 'simple-column-family)) &key)
  (let ((cf (sap-alien self (* rocksdb-column-family-handle))))
    (make-instance new-class :db cf :name (%cf-name cf))))

(defmethod change-class ((self column) (new-class (eql 'simple-column-family)) &key name)
  (let ((ret (make-instance new-class :type (column-type self))))
    (when name (setf (name ret) name))
    ret))

;;; Transactions
(defmethods transaction 
  (((self trdb) &key (write-opts (default-rocksdb-writeoptions))
                name
                (txn *transaction*)
                (opts (default-rocksdb-transaction-options)))
   (unless-null-db () self
     (let ((obj (rocksdb-transaction-begin (sap self) write-opts opts txn)))
       (when name (%set-transaction-name obj name))
       obj)))
  (((self otrdb)
    &key
    (txn *transaction*)
    (opts (default-rocksdb-optimistictransaction-options))
    (write-opts (default-rocksdb-writeoptions)))
   (unless-null-db () self
     (rocksdb-optimistictransaction-begin (db self) write-opts opts txn))))

(defmethod execute ((self rdb) (fn function) &key (txn *transaction*))
  (funcall fn)
  (when txn
    (commit txn)
    (rocksdb-transaction-destroy txn)))

;;; SST File Writer
(defstruct sst-file-writer
  (path nil :type (or null pathname))
  (sap (%sst-filewriter) :type (alien (* rocksdb-sstfilewriter))))

(defaccessor sap ((self sst-file-writer)) (sst-file-writer-sap self))
(defaccessor path ((self sst-file-writer)) (sst-file-writer-path self))

(defmethod size ((self sst-file-writer)) (%sst-file-size (sst-file-writer-sap self)))

(defmethod open-db ((self sst-file-writer))
  (%open-sst-writer (sst-file-writer-sap self) (namestring (sst-file-writer-path self))))

(defmethod close-db ((self sst-file-writer) &key)
  (%finish-sst-writer (sst-file-writer-sap self)))

(defmethod free ((self sst-file-writer))
  (with-slots (sap) self
    (unless (null sap)
      (setf (sap self) (%destroy-sst-writer sap)))))

(defmethod print-object ((self sst-file-writer) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":path ~A ~@{:size ~A~}" (sst-file-writer-path self)
            (when (sst-file-writer-sap self) (size self)))))

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

;;; Collections
(defclass rdb-collection (database-collection)
  ((collection :initform (coerce nil db::*database-collection-type*))))
