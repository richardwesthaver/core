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
                                            "WITH-ERRPTR signaled: ~A"
                                            condition)))))
            (progn ,@body))
       (handle-errptr ,e ,errtyp ,params))))

(defmacro with-db ((db-var db) &body body)
  "Bind DB-VAR to the database object DB for the lifetime of BODY."
  `(let ((,db-var ,db))
     (handler-bind ((error (lambda (condition)
                             (error 'rdb-error
                                    :message
                                    (format nil "WITH-DB signaled: ~A" condition)))))
       ,@body)))

(defmacro with-cf ((cf-var cf) &body body)
  "Bind CF to CF-VAR for the lifetime of BODY."
  `(let ((,cf-var ,cf))
     (handler-bind ((error (lambda (condition)
                             (error 'cf-error
                                    :message
                                    (format nil "WITH-CF signaled: ~A" condition)))))
       ,@body)))

(defmacro do-cfs ((cf cfs) &body body)
  "Do BODY for each CF in the array CFS."
  (with-gensyms (%cf)
    `(loop for ,%cf across ,cfs
           do (with-cf (,cf ,%cf) ,@body))))

(defmacro with-iter ((iter-var iter) &body body)
  "Bind object ITER to ITER-VAR for the lifetime of BODY.

((%ITER ITER) BODY) is passed to ROCKSDB:WITH-ITER-RAW, binding the
raw handle to the same symbol prefixed with '%'."
  (let ((%iter-var (symbolicate '% iter-var)))
    `(let ((,iter-var ,iter))
       (with-iter-raw (,%iter-var ,iter-var)
         ,@body))))  

;; TODO: sb-ext:with-current-source-form ?
(defmacro do-db ((db opts) accessors &body body)
  "Database Iteration construct. OPTS are used to provide top-level
  options dynamically bound to DB. ACCESSORS is a list of database
   accessors which are available to call in BODY."
  )
