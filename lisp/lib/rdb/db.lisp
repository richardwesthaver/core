;;; rdb/db.lisp --- RDB Database API

;; RocksDB Implementation of OBJ/DB protocol.

;;; Code:
(in-package :rdb)

(defmethod make-db ((engine (eql :rocksdb)) &rest initargs)
  (declare (ignore engine))
  (funcall 'make-rdb initargs))

(defmethod connect-db ((db rdb) &key) db)

(defmethod db-query ((db rdb) (query (eql :get)) &key key &allow-other-keys)
  (declare (ignore query))
  (get-key db key))

(defclass rdb-collection (database-collection)
  ((collection :initform (coerce nil db::*default-database-collection-type*))))
