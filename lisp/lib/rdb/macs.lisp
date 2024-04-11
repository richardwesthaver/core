;;; rdb/macs.lisp --- macros

;;; Code:
(in-package :rdb)

;;; error handling
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

;;; opts
(defmacro rdb-opt-setter (key)
  `(find-symbol (format nil "~:@(rocksdb-options-set-~x~)" ,key) :rocksdb))

(defmacro rdb-opt-getter (key)
  `(find-symbol (format nil "~:@(rocksdb-options-get-~x~)" ,key) :rocksdb))

;;; db
(defmacro with-open-db-raw ((db-var db-path &optional (opt (default-rocksdb-options))) &body body)
  `(let ((,db-var (open-db-raw ,db-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-close ,db-var)
       (with-errptr (err 'rocksdb-error)
         ;; (rocksdb-destroy-db ,opt ,db-path err) ;; when :destroy only
         (rocksdb-options-destroy ,opt)))))

(defmacro with-db ((db-var db) &body body)
  "Bind DB-VAR to the database object DB for the lifetime of BODY."
  `(let ((,db-var ,db))
     (handler-bind ((error (lambda (condition)
                             (error 'rdb-error
                                    :message
                                    (format nil "WITH-DB signaled: ~A" condition)))))
       ,@body)))

;;; cf
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

;;; iter
(defmacro with-iter-raw ((iter-var db &optional (opt (rocksdb-readoptions-create))) &body body)
  `(let ((,iter-var (create-iter-raw ,db ,opt)))
     (unwind-protect (progn ,@body)
       (destroy-iter-raw ,iter-var))))

(defmacro with-iter ((iter-var iter) &body body)
  "Bind object ITER to ITER-VAR.

((%ITER ITER) BODY) is passed to ROCKSDB:WITH-ITER-RAW, binding the
raw handle to the same symbol prefixed with '%'.

Errors that occur in the inner body will be handled but the iterator
handle will not be freed on exit."
  (let ((%iter-var (symbolicate '% (symbol-name iter-var))))
    `(let ((,iter-var ,iter))
       (let ((,%iter-var (rdb-iter-sap ,iter-var)))
         ,@body))))

;; TODO: sb-ext:with-current-source-form ?
;;; backup
(defmacro with-open-backup-engine-raw ((be-var be-path &optional (opt (rocksdb-options-create)))
                                       &body body)
  `(let ((,be-var (open-backup-engine-raw ,be-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-backup-engine-close ,be-var))))

;;; top-level
(defmacro do-db ((db opts) accessors &body body)
  "Database Iteration construct. OPTS are used to provide top-level
  options dynamically bound to DB. ACCESSORS is a list of database
   accessors which are available to call in BODY."
  )

;;; temp-db
(defvar *temp-db-path-generator*
  (lambda (&optional (name "temp-db"))
    (make-pathname :directory "tmp" :name (symbol-name (gensym name))))
  "A single arg function returning the absolute path to a temp-db path.")

(defvar *temp-db-destroy-default* t)

(defmacro with-temp-db ((db-var (&rest cfs) &key (destroy *temp-db-destroy-default*)) &body body)
  "Bind DB-VAR to a temporary RDB object, arranging for CF-VARS to be
created as column-families and destroying the database after executing
the forms in BODY."
  (setf cfs
        (mapcar
         (lambda (var)
           (setf var (make-rdb-cf (symbol-name var))))
         cfs))
  `(with-db (,db-var (make-rdb
                      (namestring (funcall ,*temp-db-path-generator* ,(symbol-name db-var)))
                      (default-rdb-opts)
                      (make-array ,(length cfs) :element-type 'rdb-cf :initial-contents ',cfs)))
       (prog1
           (progn ,@body)
         ,(if destroy
              `(destroy-db ,db-var)
              `(shutdown-db ,db-var)))))
