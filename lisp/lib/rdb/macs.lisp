(in-package :rdb)

(defmacro with-db ((db-var db) &body body)
  `(let ((,db-var ,db)) ,@body))

(defmacro with-cf ((cf-var cf) &body body)
  `(let ((,cf-var ,cf))
    ,@body))

(defmacro with-iter ((iter-var db &optional opt) &body body)
  `(let ((,iter-var (create-iter ,db ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-iter-destroy ,iter-var))))
