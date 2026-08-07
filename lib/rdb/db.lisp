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

(set-database-backend :rocksdb *rocksdb-backend-options*
                      (lambda () (load-rocksdb *save-database-backend-on-load*)))

(set-database-backend :rdb *rdb-backend-options*
                      (lambda () (db::%load-database-backend :rocksdb)))

(defmethod load-opts ((db rdb) &key)
  (with-latest-options (name db) (db-opts cf-names cf-opts)
       (let ((cfs (coerce 
                   (loop for name across cf-names
                         for opt across cf-opts
                         collect (make-rdb-cf name :opts opt))
                   'vector)))
         (setf (options db) db-opts)
         cfs)))

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

(defmethod query ((db rdb) (query (eql :get)) &key key column &allow-other-keys)
  (declare (ignore query))
  (get-val db key :column column))

;;; Database
(defclass rdb (database)
  ((options :initform (default-rocksdb-options) :accessor options))
  (:documentation "RocksDB database class.
OPTIONS is an alien ROCKSDB-OPTIONS pointer."))

(defclass trdb (rdb)
  ((transaction-options :initform (rocksdb-transaction-options-create) :accessor transaction-options))
  (:documentation "Transaction DB.
TRANSACTION-OPTIONS is an alien ROCKSDB-TRANSACTIONDB-OPTIONS pointer."))

(defclass otrdb (rdb) ()
  (:documentation "Optimistic Transaction DB."))

(defclass simple-rdb (rdb)
  ((backup :initform nil :type (or null rocksdb-backup-engine) :initarg :backup :accessor db-backup)
   (snapshots :initform (make-array 0 :element-type 'rdb-snapshot :adjustable t)
              :type (vector (alien rocksdb-snapshot))
              :initarg :snapshots 
              :accessor db-snapshots)
   (checkpoints :initform (make-array 0 :adjustable t)
                :type (vector (alien rocksdb-checkpoint))
                :initarg :checkpoints
                :accessor db-checkpoints)
   (secondary :initform nil :type (or null rocksdb) :initarg :secondary :accessor secondary-db)
   (columns :initarg :columns :accessor columns))
  (:default-initargs 
   ;; Note that we don't pre-populate this slot with the 'default' column
   ;; which is present on creation of a RocksDB database. Usually there isn't
   ;; much need to access this column directly as you can just access the
   ;; database directly, which will access the default column internally.
   :columns (make-array 0 :element-type 'rdb-column-family
              :adjustable t
              :fill-pointer t)))

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
        (map 'vector (lambda (x) (make-instance 'rdb-column-family :cf x))
             (load-opts (db self) :backfill backfill)))
  self)

(defmethod merge-columns ((self rdb) (columns vector))
  (loop for c across columns
        do (if-let ((found (find-column c self)))
             (setf (aref (columns self) (position found (columns self))) c)
             (vector-push-extend c (columns self)))))

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
  (vector-push-extend (make-instance 'column-family :db cf) (columns db)))

(defmethod open-with-columns ((db rdb) &rest names)
  (let ((cols 
          (coerce
           (if (null names)
               (columns db)
               (loop for n in names
                     collect (if-let ((col (find-column n db)))
                               col
                               (add-column 
                                (make-instance 'column-family 
                                  :db (%create-cf (db db) n))
                                db))))
           'vector)))
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
  (((self rdb-database) key val &key column)
   (if-let ((column (and column (find-column column self))))
     (if-let ((sap (sap column)))
       (%put-cf
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
       (%put-cf
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
       (%put-cf
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
       (%put-cf
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

(defmethod iter ((self rdb) &key column (opts (rocksdb-readoptions-create)))
  (etypecase column
    (column-family (rocksdb-create-iterator-cf (db self) opts (db column)))
    (null (rocksdb-create-iterator (db self) opts))
    (symbol (rocksdb-create-iterator-cf (db self) opts (cf (find-column column self))))
    (simple-string (rocksdb-create-iterator-cf (db self) opts (cf (find-column column self))))))

(defmethods get-val 
  (((self rdb) (key string) &key (opts (rocksdb-readoptions-create)) column)
   (let ((sap (db self)))
     (if column
         (%get-cf-str sap (db (find-column column self)) key opts)
         (%get-kv-str sap key opts))))
  (((self rdb) key &key (opts (rocksdb-readoptions-create)) column)
   (let ((sap (db self)))
     (if column
         (%get-cf sap (db (find-column column self)) key opts)
         (%get-kv sap key opts)))))

(defmethod multi-get ((self rdb) keys &key (data-type 'octet-vector) (opts (rocksdb-readoptions-create)) columns)
  (multi-get (db self) keys :data-type data-type :opts opts :columns (mapcar 'db columns)))

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


(defaccessor opts ((self rdb)) (options self))
;; TODO
(defaccessor opt ((self rdb) key) (opt (opts self) key))
(defmethods prop 
  (((self rdb) (name string))
   (unless-null-db () self
     (rocksdb-property-value db name)))
  (((self rdb) (name symbol))
   (prop self (string-downcase (concatenate 'string "rocksdb." (symbol-name name))))))

(defmethod print-stats ((self rdb) &optional stream)
  (if stream
      (println (rocksdb-options-statistics-get-string (options self)) stream)
      (with-output-to-string (s)
        (print-stats self s))))

(defmethod db-metadata ((self rdb) &optional column)
  (make-rdb-cf-metadata :sap (%get-metadata (db self) (db column))))

(defmethod db-stats ((self rdb) &optional (htype (rocksdb-statistics-level "all")))
  (make-rdb-stats (%get-stats (options self) htype)))

(defmethod ingest-db ((self rdb) (files list) &key column (opts (rocksdb-ingestexternalfileoptions-create)))
  (if column
      (%ingest-db-cf (db self) (db column) files opts)
      (%ingest-db (db self) files opts)))

(defmethods make-db 
  (((engine (eql :rdb)) &rest initargs &key columns &allow-other-keys)
   (declare (ignore engine))
   (remf initargs :columns)
   (make-instance 'rdb-database :db (apply 'make-db :rocksdb initargs) :columns columns))
  (((engine (eql :rdb-backup)) &key path (db *db*))
   (setf (db-backup db) (backup-db db :path path)))
  (((engine (eql :rdb-transaction)) &rest initargs &key columns &allow-other-keys)
   (remf initargs :columns)
   (let ((db (make-instance 'rdb-database :db (apply 'make-db :rocksdb-transaction initargs))))
     (when columns (setf (columns db) (coerce (mapcar (lambda (x) (cf x)) columns) 'vector)))
     db))
  (((engine (eql :rdb-secondary)) &key path opts (db *db*))
   (setf (secondary-db db) (open-secondary-db db :opts opts :path path))))

(defmethod derive-schema ((self rdb))
  (apply 'make-schema
         (loop for c across (columns self)
               collect (field-from-cf (db c)))))

(defmethod open-db ((self rdb-database))
  (with-slots (path db options) self
    (if db
        (progn
          (cerror "Ignore and continue" 'open-db-error 
                  :db db
                  :message "Database is already open")
          db)
        (setf db (%open-db path options)))))

(defmethod open-backup-engine ((self rdb-database) &key path) 
  (setf (db-backup self) (open-backup-engine (db self) :path path)))

(defmethod open-secondary-db ((self rdb-database) &key path opts) 
  (setf (secondary-db self) (open-secondary-db (db self) :opts opts :path path)))

(defmethod open-checkpoint-db ((self rdb-database) &key path)
  (vector-push-extend (%make-checkpoint (sap self) path) (db-checkpoints self)))

(defmethod snapshot-db ((self rdb))
  (unless-null-db () self
    (make-rdb-snapshot :sap (%create-snapshot db))))
(defmethod snapshot-db :around ((self simple-rdb))
   (vector-push-extend 
    (call-next-method self)
    (db-snapshots self)))

(defmethod restore-db ((self rdb) (from string) &key id opts)
  (unless-null-db (path) self
    (%restore-from-backup (open-backup-engine self :path from) path from id opts)))

(defmethod backup-db ((self rdb) &key path)
  (unless-null-db (options) self
    (if (null path)
        (error 'open-backup-engine-error :db db
                                         :message "PATH must not be nil when no backups exist")
        (%create-new-backup (open-backup-engine self :path path) db))))
(defmethod backup-db :around ((self simple-rdb) &rest args)
  (setf (db-backup self) (apply 'call-next-method args)))
(defmethod flush-db ((self rdb) &key wait)
  (%flush-db (db self) wait))
(defmethod close-db ((self rdb) &key) 
  (close-columns self)
  (%close-db (db self))
  (rocksdb-options-destroy (options self)))

(defmethod print-object ((self rdb) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":open ~A" (db-open-p self))))

(defmethod db-open-p ((self rdb))
  (with-slots (db) self
    (and db (typep db 'alien) (not (null-pointer-p db)))))

(defmethod db-closed-p ((self rdb))
  (consp (db self)))

(defmethod destroy-db ((self rdb))
  ;; close all handles before destruction ensues
  (close-db self)
  (%destroy-db (namestring (path self))))

(defmethod close-backup-engine ((self rdb-database))
  (with-slots (backup) self
    (unless (null backup)
      (setf backup (close-backup-engine backup)))))

(defmethod shutdown-db ((self rdb) &key wait)
  (log:trace! "shutting down database" (path self))
  (close-backup-engine self)
  (close-columns self)
  (when-let ((db (db self)))
    (rocksdb-cancel-all-background-work db wait)
    (close-db self)))

(defmethod get-value (elt (self rdb-database))
  (get-value elt (db self)))

(defmethod put-key ((self rdb-database) key val)
  (put-key (db self) key val))

(defmethod delete-key ((self rdb-database) key &key)
  (delete-key (db self) key))

(defmethod merge-key ((self rdb-database) key val &key (opts (rocksdb-writeoptions-create)))
  (merge-key (db self) key val :opts opts))

(defmethod add-column (col (self rdb-database))
  (vector-push-extend col (coerce (columns self) 'vector)))

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
               (make-instance 'rdb-column-family :db (%create-cf (db self) (name field)) :type (field-type field))
               field)
              self))
        finally (return self)))

;;; Column Families
(defclass column-family (rdb)
  ((name :initform "default" :initarg :name :accessor name))
  (:documentation "RocksDB Column Family.
Inherits directly from the RDB class. The DB slot is a
ROCKSDB-COLUMN-FAMILY-HANDLE."))

;; rename this to CF (or maybe column-family), remove dependency on RDB-COLUMN
(defclass rdb-column-family (column-family rdb-column)
  (:default-initargs :name (symbol-name (gensym "#")))
  (:documentation "High-level Lisp-side RocksDB Column Family base class. Implements the COLUMN
protocol and contains a CF slot which contains an RDB-CF structure
object. (SAP CF) is the raw pointer."))

(defaccessor name ((self column-family)) (%cf-name (db self)))
(defaccessor sap ((self column-family)) (db self))
(defaccessor id ((self column-family)) (%cf-id (db self)))

(defun schema-from-rdb-column-families (columns)
  "Convert a sequence of RDB-COLUMN-FAMILYs to a SCHEMA."
  (apply 'make-schema 
     (map 'list 
          (lambda (x)
        (make-field :name (keywordicate (name x)) :type (column-type x)))
        columns)))

(defmethod free ((self column-family))
  (with-slots (db) self (unless (null db) (setf (db self) (%destroy-cf db)))))

(defmethod close-db ((self column-family))
  (free self))

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
  (make-instance new-class :cf (make-rdb-cf (name self)) :type (field-type self)))

(defmethod change-class ((self rdb-cf) (new-class (eql 'rdb-column-family)) &key)
  (make-instance new-class :cf self))

(defmethod change-class ((self column) (new-class (eql 'rdb-column-family)) &key name)
  (let ((ret (make-instance new-class :type (column-type self))))
    (when name (setf (name ret) name))
    ret))

;;; Transactions
(defmethod make-transaction ((self rdb-database)
                             &key (write-opts (rocksdb-writeoptions-create))
                                  (name (name self))
                                  txn
                                  (opts (rocksdb-transaction-options-create)))
  (with-errptr e
    (let ((txn-db (db self)))
      (let ((obj (make-transaction txn-db 
                                   :write-opts write-opts 
                                   :opts opts 
                                   :txn txn)))
        (when name (setf (name obj) name))
        obj))))

(defmethod execute-transaction ((self rdb-database) (fn function) &key (txn *transaction*))
  (funcall fn)
  (when txn
    (commit-transaction txn)
    (rocksdb-transaction-destroy txn)))

;;; Collections
(defclass rdb-collection (database-collection)
  ((collection :initform (coerce nil db::*database-collection-type*))))
