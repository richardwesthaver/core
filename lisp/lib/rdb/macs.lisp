;;; rdb/macs.lisp --- macros

;;; Code:
(in-package :rdb)

;;; error handling
(defmacro with-errptr* ((e err &rest params) &body body)
  `(with-errptr ,e
     (handler-bind ((sb-sys:memory-fault-error
                      (lambda (c)
                        (declare (ignore c))
                        (rocksdb-c-error ,e)))
                    (error (lambda (c)
                             (declare (ignore c))
                             (error ,err :message (deref (sap-alien ,e (* c-string))) ,@params))))
       (progn ,@body))))

;;; opts
(macrolet ((%def-opt-finders (name opt)
             `(progn 
                (defmacro ,(symbolicate name '-setter) (key)
                  `(find-symbol (format nil "~:@(~A-SET-~A~)" ',',opt ,key) :rocksdb))
                (defmacro ,(symbolicate name '-getter) (key)
                  `(find-symbol (format nil "~:@(~A-GET-~A~)" ',',opt ,key) :rocksdb)))))
  (%def-opt-finders rdb-opt rocksdb-options)
  (%def-opt-finders rdb-writeopt rocksdb-writeoptions)
  (%def-opt-finders rdb-readopt rocksdb-readoptions)
  (%def-opt-finders rdb-compactopt rocksdb-compactoptions)
  (%def-opt-finders rdb-backupopt rocksdb-backup-engine-options))

;;; db
(defmacro with-open-rdb-raw ((db-var db-path &optional (opt (default-rocksdb-options))) &body body)
  `(let ((,db-var (open-db-raw ,db-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-close ,db-var)
       (with-errptr* (err 'rocksdb-alien-error)
         ;; (rocksdb-destroy-db ,opt ,db-path err) ;; when :destroy only
         (rocksdb-options-destroy ,opt)))))

(defmacro with-rdb ((db-var db &key open close) &body body)
  "Bind DB-VAR to the database object DB for the lifetime of BODY."
  `(let ((,db-var ,db))
     (handler-bind ((error (lambda (condition)
                             (error 'rdb-error
                                    :message
                                    (format nil "WITH-RDB signaled: ~A" condition)))))
       ,@(when open `(open-db ,db-var))
       ,@(if close `(unwind-protect (progn ,@body) (close-db ,db-var))
             body))))

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
         (declare (ignorable ,%iter-var))
         ,@body))))

;; TODO: sb-ext:with-current-source-form ?
;;; backup
(defmacro with-open-backup-engine-raw ((be-var be-path &optional (opt (rocksdb-options-create)))
                                       &body body)
  `(let ((,be-var (open-backup-engine-raw ,be-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-backup-engine-close ,be-var))))

;;; top-level
;; TODO 2024-09-26: 
(defmacro do-db ((db opts) accessors &body body)
  "Database Iteration construct. OPTS are used to provide top-level
  options dynamically bound to DB. ACCESSORS is a list of database
   accessors which are available to call in BODY.")

;;; temp-db
(defvar *temp-db-path-generator*
  (lambda (&optional (name "temp-db"))
    (make-pathname :directory "tmp" :name (symbol-name (gensym name))))
  "A single arg function returning the absolute path to a temp-db path.")

(defvar *temp-db-destroy* nil)

(defmacro with-temp-rdb ((db-var (&rest cfs) &key (destroy *temp-db-destroy*) open) &body body)
  "Bind DB-VAR to a temporary RDB object, arranging for CF-VARS to be
created as column-families and destroying the database after executing
the forms in BODY."
  (setf cfs
        (mapcar
         (lambda (var)
           (setf var (make-rdb-cf (symbol-name var))))
         cfs))
  `(with-rdb (,db-var (make-rdb
                       :name (namestring (funcall ,*temp-db-path-generator* ,(symbol-name db-var)))
                       :opts (default-rdb-opts)
                       :cfs (make-array ,(length cfs) :element-type 'rdb-cf 
                                                      :initial-contents ',cfs 
                                                      :adjustable t 
                                                      :fill-pointer ,(length cfs))))
     ,@(when open `((open-db ,db-var)
                    (create-columns ,db-var)))
       (prog1
           (progn ,@body)
         ,(if destroy
              `(destroy-db ,db-var)
              `(shutdown-db ,db-var)))))
;;; sst
(defmacro with-sst ((sst &key file comparator destroy) &body body)
  "Do BODY with SST bound to a SST-FILE-WRITER. When FILE is supplied
the writer will automatically open that file.

When COMPARATOR is supplied it is used as the comparator function for
the writer. Every key inserted MUST be in ascending order, according
to the comparator. By default the ordering is binary
lexicographically.

It is up to the developer to ensure that the comparator used by a
writer is exactly the same as the comparator used when ingesting the
file by a RDB instance."
  `(let ((,sst (make-sst-file-writer ,comparator)))
     ,@(when file `((open-sst ,sst ,file)))
     ,@body
     ,@(when destroy `((destroy-sst ,sst)))))

;;; opts
(defmacro with-latest-opts (db &body body)
  `(progn
     (let ((,db (load-opts ,db)))
       ,@body)))
