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

(defmethod make-db ((engine (eql :rocksdb)) &rest initargs)
  (declare (ignore engine))
  (funcall 'make-rdb initargs))

(defmethod connect-db ((db rdb) &key) db)

(defmethod query-db ((db rdb) (query (eql :get)) &key key &allow-other-keys)
  (declare (ignore query))
  (get-key db key))

(defclass rdb-collection (database-collection)
  ((collection :initform (coerce nil db::*default-database-collection-type*))))
