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
         (setf (options db) (make-rdb-opts* db-opts))
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

;;; Column Families
;; rename this to CF (or maybe column-family), remove dependency on RDB-COLUMN
(defclass rdb-column-family (rdb-column) 
  ((cf :initarg :cf :type rdb-cf :accessor cf))
  (:default-initargs :cf (make-rdb-cf (symbol-name (gensym "#"))))
  (:documentation "High-level Lisp-side RocksDB Column Family base class. Implements the COLUMN
protocol and contains a CF slot which contains an RDB-CF structure
object. (SAP CF) is the raw pointer."))

(defaccessor name ((self rdb-column-family)) (name (cf self)))
(defaccessor sap ((self rdb-column-family)) (sap (cf self)))
(defaccessor options ((self rdb-column-family)) (rdb-cf-opts (cf self)))

(defun schema-from-rdb-column-families (columns)
  "Convert a sequence of RDB-COLUMN-FAMILYs to a SCHEMA."
  (apply 'make-schema 
	 (map 'list 
	      (lambda (x)
		(make-field :name (keywordicate (name x)) :type (column-type x)))
		columns)))

(defmethod free ((self rdb-column-family))
  (with-slots (sap) self (unless (null sap) (setf sap (%destroy-cf sap)))))

(defmethod close-column ((self rdb-column-family) &optional error)
  (close-column (sap self) error))

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

;;; Database
(defclass rdb-database (database)
  ((options :initform (default-rocksdb-options) :accessor options)))

(defclass simple-rdb (rdb-database)
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
  (((db rdb-database) (key (eql :close)) (val (eql :auto)))
   "Arrange for SHUTDOWN-DB to be called when there are no more references to DB."
   (sb-ext:finalize db (lambda () (close-db db))))
  (((db rdb-database) (key (eql :merge-op)) val)
   "Assign a MERGE-OP to this database."
   (setf (opt db :merge-operator) val))
  (((db rdb-database) (key (eql :comparator)) val)
   "Assign a custom COMPARATOR to this database."
   (setf (opt db :comparator) val))
  (((db rdb-database) (key (eql :prefix-op)) val)
   "Assign a custom SLICETRANSFORM to this database to be used as a prefix
extractor."
   (setf (opt db :prefix-extractor) val))
  (((db rdb-database) (key (eql :event-listener)) val)
   "Assign an EVENT-LISTENER to this database."
   (setf (opt db :event-listener) val)))

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

(defmethod reset ((self rdb-database) &key (columns t) (opts (default-rocksdb-options)))
  (when columns 
    (close-columns self) 
    (setf (columns self)
          (make-array 0 :element-type 'rdb-column-family
                        :adjustable t
                        :fill-pointer t)))
  (setf (options self) opts)
  self)

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
    (multiple-value-bind (db-sap cfs) (%open-cfs (opts db) (name db)
                                                    (loop for c across cols
                                                          collect (name c))
                                                    (loop for c across cols
                                                          collect (sap (options c))))
      (setf (sap db) db-sap)
      (loop for c across cfs
            do (when-let ((col (find-column (name c) db)))
                 (setf (sap (cf col)) c)))
      db)))

(defmethod open-columns* ((self rdb-database))
  (let ((names) (opts))
    (loop for c across (columns self)
          do (push (name c) names)
          do (push (sap (options c)) opts))
    (nreversef names)
    (nreversef opts)
    (unless (member *rdb-default-column-name* names :test 'string=)
      (push *rdb-default-column-name* names)
      (push (opts self) opts))
    (multiple-value-bind (db cfs)
        (%open-cfs (opts self) (name self) names opts)
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
         (%get-cf-str sap (sap (find-column column self)) key opts)
         (%get-kv-str sap key opts))))
  (((self rdb-database) key &key (opts (rocksdb-readoptions-create)) column)
   (let ((sap (sap self)))
     (if column
         (%get-cf sap (sap (find-column column self)) key opts)
         (%get-kv sap key opts)))))

(defmethod multi-get ((self rdb-database) keys &key (data-type 'octet-vector) (opts (rocksdb-readoptions-create)) columns)
  (multi-get (db self) keys :data-type data-type :opts opts :cf (mapcar 'cf columns)))

(defmethod create-column ((db rdb-database) (col rdb-column-family))
  (if (equal (name col) *rdb-default-column-name*)
      (rdb-default-column-warning "ignoring attempt to create 'default' column-family: ~A" col)
      (setf (sap col) (%create-cf (sap db) (name col) (sap (options col)))))
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
  (prop self "rocksdb.current-super-version-number"))

(defaccessor name ((self rdb-database)) (path self))
(defaccessor sap ((self rdb-database)) (db self))
(defaccessor opts ((self rdb-database)) (options self))
(defaccessor opt ((self rdb-database) key) (opt (opts self) key))
(defmethods prop 
  (((self rdb-database) (name string))
   (unless-null-db () self
     (rocksdb-property-value db name)))
  (((self rdb-database) (name symbol))
   (prop self (string-downcase (concatenate 'string "rocksdb." (symbol-name name))))))

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
  (((engine (eql :rdb-transaction)) &rest initargs &key columns &allow-other-keys)
   (remf initargs :columns)
   (let ((db (make-instance 'rdb-database :db (apply 'make-db :rocksdb-transaction initargs))))
     (when columns (setf (columns db) (coerce (mapcar (lambda (x) (cf x)) columns) 'vector)))
     db))
  (((engine (eql :rdb-secondary)) &key path opts (db *db*))
   (setf (secondary-db db) (open-secondary-db db :opts opts :path path))))

(defmethod derive-schema ((self rdb-database))
  (apply 'make-schema
         (loop for c across (columns self)
               collect (field-from-cf (cf c)))))

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

(defmethod shutdown-db ((self rdb-database) &key) 
  (close-backup-engine self)
  (close-columns self)
  (shutdown-db (db self)))

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

(defmethod close-columns ((self rdb-database))
  (with-slots (columns) self
    (loop for cf across columns
          do (setf cf (close-column cf)))))

(defmethod load-schema ((self rdb-database) (schema schema))
  "Load SCHEMA into rdb database object SELF. This will add any missing rdb-cfs
and update existing key/value types for cfs with the same name. Existing cfs
only get their type slots updated on non-nil values."
  (loop for field across (fields schema)
        do (if-let ((col (find-column (name field) self)))
             (load-field col field)
             (add-column
              (load-field
               (make-instance 'rdb-column-family :cf (make-rdb-cf (name field)) :type (field-type field))
               field)
              self))
        finally (return self)))

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
