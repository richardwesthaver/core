;;; rdb/db.lisp --- RDB Database API

;; RocksDB Implementation of OBJ/DB protocol.

;;; Code:
(in-package :rdb)

(defvar *rocksdb-backend-options* '(columns temp path (open t) 
                                    destroy (close t) backup secondary 
                                    snapshots sap))
(defvar *rdb-backend-options* (append *rocksdb-backend-options* '(store schema)))

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
                              (setf (rdb-opts-sap cf-opts) opt)
                              (make-rdb-cf name :opts cf-opts)))
                   'vector)))
         (setf (rdb-opts db) (make-rdb-opts* db-opts)
               (rdb-cfs db) cfs))))

(defmethod make-db ((engine (eql :rocksdb)) &rest initargs &key 
                    (name #.(string-downcase (gensym "RDB")))
                    (opts (default-rdb-opts)))
  (declare (ignore engine))
  (apply 'make-rdb name opts initargs))

(defmethod connect-db ((db rdb) &key) db)

(defmethod query-db ((db rdb) (query (eql :get)) &key key &allow-other-keys)
  (declare (ignore query))
  (get-val db key))

(defclass rdb-database (database) ()
  (:default-initargs 
   :db (make-db :rocksdb)))

(defmethod name ((self rdb-database))
  (name (db self)))

(defmethod (setf name) (new (self rdb-database))
  (setf (name (db self)) new))

(defmethod columns ((self rdb-database))
  (columns (db self)))

(defmethod (setf columns) (new (self rdb-database))
  (setf (columns (db self)) new))

(defmethod sap ((self rdb-database))
  (sap (db self)))

(defmethod (setf sap) (new (self rdb-database))
  (setf (sap (db self)) new))

(defmethod db-opts ((self rdb-database))
  (db-opts (db self)))

(defmethod (setf db-opts) (new (self rdb-database))
  (setf (db-opts (db self)) new))

(defmethod make-db ((engine (eql :rdb)) &rest initargs &key name columns opts sap)
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

(defmethod open-db ((self rdb-database)) (open-db (db self)))

(defmethod flush-db ((self rdb-database) &rest args &key) (apply 'flush-db (db self) args))

(defmethod close-db ((self rdb-database) &key) (close-db (db self)))

(defmethod destroy-db ((self rdb-database)) (destroy-db (db self)))

(defmethod shutdown-db ((self rdb-database) &key) (shutdown-db (db self)))

(defmethod get-val ((self rdb-database) elt &rest initargs &key)
  (apply 'get-val (db self) elt initargs))

(defmethod get-value (elt (self rdb-database))
  (get-value elt (db self)))

(defmethod start-transaction ((self rdb-database) transaction 
                              &key (write-opts (rocksdb-writeoptions-create))
                                   (transaction-opts (rocksdb-transaction-options-create)))
  (with-errptr e
    (rocksdb-transaction-prepare 
     (rocksdb-transaction-begin write-opts transaction-opts nil) 
     e)))

(defmethod commit-transaction ((self rdb-database) txn &key)
  (with-errptr e
    (rocksdb-transaction-commit txn e)))

(defmethod abort-transaction ((self rdb-database) txn &key)
  (with-errptr e
    (rocksdb-transaction-rollback txn e)
    (rocksdb-transaction-destroy txn)))

(defmethod execute-transaction ((self rdb-database) txn &key)
  (commit-transaction self txn))

(defclass rdb-collection (database-collection)
  ((collection :initform (coerce nil db::*default-database-collection-type*))))
