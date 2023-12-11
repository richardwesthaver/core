(in-package :rdb)

(defmacro with-db ((db-var db) &body body)
  `(let ((,db-var ,db)) ,@body))

(defmacro with-cf ((cf-var cf) &body body)
  `(let ((,cf-var ,cf))
    ,@body))
