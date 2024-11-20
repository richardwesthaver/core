;;; rdb/db.lisp --- RDB Database API

;; RocksDB Implementation of OBJ/DB protocol.

;;; Code:
(in-package :rdb)

;;; Backend
(defvar *rocksdb-backend-options* '(columns temp path (open t) 
                                    destroy (close t) backup secondary
                                    snapshots sap))

(defvar *rdb-backend-options* (append *rocksdb-backend-options* '(store schema)))

(defmethod set-database-backend-option ((db rdb) (key (eql :close)) (val (eql :auto)))
  "Arrange for SHUTDOWN-DB to be called when there are no more references to DB."
  (sb-ext:finalize db (lambda () (shutdown-db db))))

(set-database-backend :rocksdb *rocksdb-backend-options*
                      #'load-rocksdb)

(set-database-backend :rdb *rdb-backend-options*
                      (lambda () (db::%load-database-backend :rocksdb)))

(defmethod load-opts ((db rdb))
  (with-latest-options (rdb-name db) (db-opts cf-names cf-opts)
       (let ((cfs (coerce 
                   (loop for name across cf-names
                         for opt across cf-opts
                         collect 
                            (let ((cf-opts (make-rdb-opts)))
                              (setf (sap cf-opts) opt)
                              (make-rdb-cf name :opts cf-opts)))
                   'vector)))
         (setf (rdb-opts db) (make-rdb-opts* db-opts)
               (rdb-cfs db) cfs))))

(defmethod make-db ((engine (eql :rocksdb)) &rest initargs &key 
                    (name #.(string-downcase (gensym "RDB")))
                    (opts (default-rdb-opts)))
  (declare (ignore engine))
  (apply 'make-rdb :name name :opts opts initargs))

(defmethod connect-db ((db rdb) &key) db)

(defmethod query-db ((db rdb) (query (eql :get)) &key key &allow-other-keys)
  (declare (ignore query))
  (get-val db key))

;;; Database
(defclass rdb-database (database)
  ((txn :initform nil :type (or null rdb-transaction-db) :initarg :txn :accessor transaction-db)
   (backup :initform nil :type (or null rdb-backup-db) :initarg :txn :accessor db-backup)
   (secondary :initform nil :type (or null rdb-backup-db) :initarg :txn :accessor secondary-db)
   (schema :initform nil :type (or null schema) :initarg :schema :accessor schema))
  (:default-initargs 
   :db (make-db :rocksdb)))

(defmethod database-version ((self rdb-database))
  "Return the version tag or nil if unmarked"
  (when-let ((db (and #1=(db self) (sap #1#))))
    (rocksdb-property-value db "rocksdb.current-super-version-number")))

(defaccessor (name) ((self rdb-database)) (name (db self)))
(defaccessor (columns) ((self rdb-database)) (columns (db self)))
(defaccessor (sap) ((self rdb-database)) (sap (db self)))
(defaccessor (db-opts) ((self rdb-database)) (db-opts (db self)))

(defmethods make-db 
  (((engine (eql :rdb)) &rest initargs &key name columns opts sap)
   (declare (ignore engine))
   (remf initargs :name)
   (remf initargs :columns)
   (remf initargs :opts)
   (let ((db (apply 'make-instance 'rdb-database initargs)))
     (when name (setf (name db) name))
     (when columns (setf (columns db) columns))
     (when opts (setf (db-opts db) opts))
     (when sap (setf (sap db) sap))
     db))
  (((engine (eql :rdb-backup)) &key path (db *db*))
   (setf (db-backup db) (backup-db db :path path)))
  (((engine (eql :rdb-transaction)) &key path opts (db *db*))
   (setf (transaction-db db) (open-transaction-db db :opts opts :path path)))
  (((engine (eql :rdb-secondary)) &key path opts (db *db*))
   (setf (secondary-db db) (open-secondary-db db :opts opts :path path))))

(defmethod open-db ((self rdb-database)) (open-db (db self)))
(defmethod open-transaction-db ((self rdb-database) &key path opts) 
  (setf (transaction-db self) (open-transaction-db (db self) :opts opts :path path)))
(defmethod open-backup-db ((self rdb-database) &key path) 
  (setf (db-backup self) (open-backup-db (db self) :path path)))
(defmethod open-secondary-db ((self rdb-database) &key path opts) 
  (setf (secondary-db self) (open-secondary-db (db self) :opts opts :path path)))

(defmethod flush-db ((self rdb-database) &rest args &key) (apply 'flush-db (db self) args))

(defmethod close-db ((self rdb-database) &key) (close-db (db self)))

(defmethod destroy-db ((self rdb-database)) (destroy-db (db self)))

(defmethod shutdown-db ((self rdb-database) &key) (shutdown-db (db self)))

(defmethod get-val ((self rdb-database) elt &rest initargs &key)
  (apply 'get-val (db self) elt initargs))

(defmethod get-value (elt (self rdb-database))
  (get-value elt (db self)))

(defmethod put-key ((self rdb-database) key val)
  (put-key (db self) key val))

(defmethod delete-key ((self rdb-database) key &key)
  (delete-key (db self) key))

(defmethod merge-key ((self rdb-database) key val &key (opts (rocksdb-writeoptions-create)))
  (merge-kv-raw (sap self) key val opts))

(defmethod merge-kv ((self rdb-database) kv &key (opts (rocksdb-writeoptions-create)))
  (merge-kv-raw (sap self) (kv-key kv) (kv-val kv) opts))

;;; Transactions
(defmethod make-transaction ((self rdb-database)
                             &key (write-opts (rocksdb-writeoptions-create))
                                  path
                                  (name (name self))
                                  txn
                                  (opts (rocksdb-transaction-options-create))
                                  (db-opts (rocksdb-transactiondb-options-create)))
  (with-errptr e
    (let ((txn-db (or (transaction-db self)
                      (setf (transaction-db self)
                            (open-transaction-db self :opts db-opts :path path)))))
      (let ((obj (make-rdb-transaction :sap (rocksdb-transaction-begin (sap txn-db) write-opts opts txn))))
        (when name (setf (name obj) name))
        obj))))
              

(defmethod execute-transaction ((self rdb-database) txn &key)
  (prog1 
      (commit-transaction txn)
    (rocksdb-transaction-destroy txn)))

;;; Collections
(defclass rdb-collection (database-collection)
  ((collection :initform (coerce nil db::*default-database-collection-type*))))
