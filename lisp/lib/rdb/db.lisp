;;; rdb/db.lisp --- RDB Database API

;; RocksDB Implementation of OBJ/DB protocol.

;;; Code:
(in-package :rdb)

;;; Backend
(defvar *rocksdb-backend-options* '(columns temp path (open . t) 
                                    destroy (close . t) 
                                    sap merge-op comparator prefix-op logger))

(defvar *rdb-backend-options* (append *rocksdb-backend-options* '(store schema backup secondary snapshots)))

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
  (((db rdb) (key (eql :logger)) val)
   (setf (db-opt db :info-log :push t) val)))

(set-database-backend :rocksdb *rocksdb-backend-options*
                      (lambda () (load-rocksdb *save-database-backend-on-load*)))

(set-database-backend :rdb *rdb-backend-options*
                      (lambda () (db::%load-database-backend :rocksdb)))

(defmethod load-opts ((db rdb))
  (with-latest-options (name db) (db-opts cf-names cf-opts)
       (let ((cfs (coerce 
                   (loop for name across cf-names
                         for opt across cf-opts
                         collect 
                            (let ((cf-opts (make-rdb-opts)))
                              (setf (sap cf-opts) opt)
                              (make-rdb-cf name :opts cf-opts)))
                   'vector)))
         (setf (rdb-opts db) (make-rdb-opts* db-opts))
         (values db cfs))))

(defmethod make-db ((engine (eql :rocksdb)) &rest initargs &key 
                    (name (string-downcase (gensym "rdb")))
                    merge-op
                    prefix-op
                    logger
                    (opts (default-rdb-opts)))
  (declare (ignore engine initargs))
  (when merge-op
    (set-db-opt opts :merge-operator merge-op :push t))
  (when prefix-op 
    (set-db-opt opts :prefix-extractor prefix-op :push t))
  (when logger
    (set-db-opt opts :info-log logger :push t))
  (make-rdb :name name :opts opts))

(defmethod query-db ((db rdb) (query (eql :get)) &key key &allow-other-keys)
  (declare (ignore query))
  (get-val db key))

;;; Column Families
(defclass rdb-column-family (rdb-column) 
  ((cf :initarg :cf :type rdb-cf :accessor cf))
  (:default-initargs :cf (make-rdb-cf (symbol-name (gensym "#"))))
  (:documentation "High-level Lisp-side RocksDB Column Family base class. Implements the COLUMN
protocol and contains a CF slot which contains an RDB-CF structure
object. (SAP CF) is the raw pointer."))

(defaccessor (name) ((self rdb-column-family)) (name (cf self)))
(defaccessor (sap) ((self rdb-column-family)) (sap (cf self)))

(defmethod destroy-column ((self rdb-column-family))
  (destroy-column (cf self)))

(defmethod open-column ((self rdb-column-family))
  (open-column (cf self)))

(defmethod close-column ((self rdb-column-family) &optional error)
  (close-column (cf self) error))

(defmethod load-field ((self rdb-column-family) (field field))
  (let ((type (field-type field))
        (ctype (column-type self)))
  (typecase type
    (null nil)
    (atom (setf (cdr ctype) type))
    (list (setf (car ctype) (car type)
                (cdr ctype)
                (if (and (listp (cdr type))
                         (= 1 (length (cdr type))))
                    (cadr type)
                    (cdr type)))))
    self))

;;; Database
(defclass rdb-database (database)
  ((txn :initform nil :type (or null rdb-transaction-db) :initarg :txn :accessor transaction-db)
   (backup :initform nil :type (or null rdb-backup-db) :initarg :txn :accessor db-backup)
   (snapshots :initform (make-array 0 :element-type 'rdb-snapshot :adjustable t)
              :type (vector rdb-snapshot)
              :initarg :snapshots 
              :accessor db-snapshots)
   (secondary :initform nil :type (or null rdb-backup-db) :initarg :txn :accessor secondary-db)
   (schema :initform nil :type (or null schema) :initarg :schema :accessor schema)
   (columns :initarg :columns :accessor columns))
  (:default-initargs 
   :db (make-db :rocksdb :opts (default-rdb-opts))
   ;; Note that we don't pre-populate this slot with the 'default' column
   ;; which is present on creation of a RocksDB database. Usually there isn't
   ;; much need to access this column directly as you can just access the
   ;; database directly, which will access this column internally.
   :columns (make-array 0 :element-type 'rdb-column-family
                          :adjustable t)))

(defmethods set-database-backend-option
  (((db rdb-database) (key (eql :close)) (val (eql :auto)))
   "Arrange for SHUTDOWN-DB to be called when there are no more references to DB."
   (sb-ext:finalize db (lambda () (shutdown-db db))))
  (((db rdb-database) (key (eql :merge-op)) val)
   "Assign a MERGE-OP to this database."
   (setf (db-opt db :merge-operator :push t) val))
  (((db rdb-database) (key (eql :comparator)) val)
   "Assign a custom COMPARATOR to this database."
   (setf (db-opt (db db) :comparator) val))
  (((db rdb-database) (key (eql :prefix-op)) val)
   "Assign a custom SLICETRANSFORM to this database to be used as a prefix
extractor."
   (setf (db-opt (db db) :prefix-extractor :push t) val)))

(defmethod find-column ((cf string) (self rdb-database) &key)
  (find cf (columns self) :key 'name :test 'equal))

(defmethod add-column ((cf rdb-cf) (db rdb-database))
  (vector-push-extend (make-instance 'rdb-column-family :cf cf) (columns db)))

(defmethod open-columns ((db rdb-database) &rest names)
  (let ((cf-names) (cf-opts))
    (loop for cf across (columns db)
          do (let ((name (name cf)))
               (when (or (not names) (member name names :test 'string=))
                   (push name cf-names)
                   (push (sap (column-opts cf)) cf-opts)))
          finally
             (setf cf-names (nreverse cf-names) 
                   cf-opts (nreverse cf-opts)))
    (multiple-value-bind (db-sap cfs) (open-cfs-raw (db-opts db) (name db) cf-names cf-opts)
      (setf (sap db) db-sap)
      (loop for cf across (columns db)
            with i = 0
            do (setf (sap cf) (deref cfs i))
            do (incf i))
      db)))

(defmethod close-columns ((self rdb-database))
  (loop for cf across (columns self)
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
  (if column
      (iter (db self) :cf (cf column) :opts opts)
      (iter (db self) :opts opts)))

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

(defmethod create-column ((db rdb-database) (col rdb-column-family))
  (setf (sap col)
        (create-cf-raw (sap db) (name col) (sap (db-opts db))))
  col)

(defmethod create-columns ((self rdb-database))
  (if (null (sap self))
      (warn 'db-missing :message "ignoring attempt to create column-families before opening")
      (loop for cf across (columns self)
            do (create-column self cf))))

(defmethod find-column ((cf string) (self rdb-database) &key)
  "Find a CF by name."
  (find cf (columns self) :key 'name :test 'equal))

(defmethod database-version ((self rdb-database))
  "Return the version tag or nil if unmarked"
  (when-let ((db (and #1=(db self) (sap #1#))))
    (rocksdb-property-value db "rocksdb.current-super-version-number")))

(defaccessor (name) ((self rdb-database)) (name (db self)))
(defaccessor (sap) ((self rdb-database)) (sap (db self)))
(defaccessor (db-opts) ((self rdb-database)) (db-opts (db self)))
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

(defmethod db-stats ((self rdb-database) &optional type)
  (db-stats (db self) type))

(defmethod ingest-db ((self rdb-database) files &key (opts (rocksdb-ingestexternalfileoptions-create))
                                                     column)
  (if column
      (ingest-db (db self) files :opts opts :cf (cf column))
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

(defmethod open-columns ((self rdb-database) &rest names) 
  (apply 'open-columns (db self) names))

(defmethod derive-schema ((self rdb-database))
  (apply 'make-schema
         (loop for c across (columns self)
               collect (translate-cf-to-field (cf c)))))

(defmethod open-db ((self rdb-database)) (open-db (db self)) self)
(defmethod open-transaction-db ((self rdb-database) &key path opts optimistic)
  (setf (transaction-db self) (open-transaction-db (db self) :opts opts :path path :optimistic optimistic)))
(defmethod open-backup-db ((self rdb-database) &key path) 
  (setf (db-backup self) (open-backup-db (db self) :path path)))
(defmethod open-secondary-db ((self rdb-database) &key path opts) 
  (setf (secondary-db self) (open-secondary-db (db self) :opts opts :path path)))

(defmethod flush-db ((self rdb-database) &rest args &key &allow-other-keys) (apply 'flush-db (db self) args))

(defmethod close-db ((self rdb-database) &key) 
  (destroy-columns self)
  (close-db (db self)))

(defmethod db-closed-p ((self rdb-database)) (db-closed-p (db self)))
(defmethod db-open-p ((self rdb-database)) (db-open-p (db self)))
(defmethod destroy-db ((self rdb-database)) (destroy-db (db self)))

(defmethod close-backup-db ((self rdb-database))
  (with-slots (backup) self
    (unless (null backup)
      (setf backup (close-backup-db backup)))))

(defmethod shutdown-db ((self rdb-database) &key) (shutdown-db (db self)))

(defmethod get-val ((self rdb-database) elt &rest initargs &key)
  (apply 'get-val (db self) elt initargs))

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
  (vector-push-extend col (columns self)))

(defmethod destroy-columns ((self rdb-database) &key &allow-other-keys)
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
               (make-instance 'rdb-column-family :cf (make-rdb-cf (field-name field))) 
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

(defmethod execute-transaction ((self rdb-database) txn &key)
  (prog1 (commit-transaction txn)
    (rocksdb-transaction-destroy txn)))

;;; Collections
(defclass rdb-collection (database-collection)
  ((collection :initform (coerce nil db::*default-database-collection-type*))))
