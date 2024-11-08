;;; rdb/db.lisp --- RDB Database API

;; RocksDB Implementation of OBJ/DB protocol.

;;; Code:
(in-package :rdb)

(defmethod load-opts ((db rdb))
  (rocksdb::with-latest-options (rdb-name db) (db-opts cf-names cf-opts)
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

(defmethod make-db ((engine (eql :rdb)) &rest initargs)
  (declare (ignore engine))
  (apply 'make-instance 'rdb-database initargs))

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
