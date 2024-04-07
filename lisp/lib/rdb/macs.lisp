;;; rdb/macs.lisp --- macros

;;; Code:
(in-package :rdb)

(defmacro with-errptr ((e &optional errtyp params) &body body)
  `(with-alien ((,e rocksdb-errptr nil))
     (unwind-protect 
          (handler-bind ((sb-sys:memory-fault-error 
                           (lambda (condition)
                             (error 'rdb-error
                                    :message
                                    (format nil
                                            "~a" condition))))
                         (error 
                           (lambda (condition)
                             (error 'rdb-error 
                                    :message 
                                    (format nil 
                                            "unhandled exception in body of WITH-ERRPTR: ~a"
                                            condition)))))
            (progn ,@body))
       (handle-errptr ,e ,errtyp ,params))))

(defmacro with-db ((db-var db) &body body)
  `(let ((,db-var ,db))
     (handler-bind ((error (lambda (condition)
                             (error 'rdb-error
                                    :message
                                    (format nil "unhandled exception in body of WITH-DB: ~a" condition)))))
       ,@body)))

(defmacro with-cf ((cf-var cf) &body body)
  `(let ((,cf-var ,cf))
     ,@body))

(defmacro do-cfs ((cf cfs) &body body)
  (with-gensyms (%cf)
    `(loop for ,%cf across ,cfs
           do (with-cf (,cf ,%cf) ,@body))))
              
