(in-package :rdb)

(defmacro with-db ((db-var db) &body body)
  `(let ((,db-var ,db))
     (handler-bind ((error (lambda (condition)
                             (close-db ,db-var)
                             (error 'rdb-error
                                    :message
                                    (format nil "unhandled exception in body of WITH-DB: ~a" condition)))))
       ,@body)))

(defmacro with-cf ((cf-var cf) &body body)
  `(let ((,cf-var ,cf))
    ,@body))
