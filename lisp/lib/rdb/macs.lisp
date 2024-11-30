;;; rdb/macs.lisp --- macros

;;; Code:
(in-package :rdb)

;;; error handling
(defmacro with-errptr* ((e err &rest params) &body body)
  "Bind e to a C pointer which can be used by alien functions, and if an error is
signaled we coerce this pointer to a string and feed it to a condition of name
ERR with initargs PARAMS for the duration of BODY."
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

;; These expand into lookup macros for the pre-defined option GET and SET
;; functions - for example RDB-OPT-SETTER and RDB-OPT-GETTER.
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

;;; rdb
;; these functions only apply to the low-level API in RDB/OBJ (structs only)
(defmacro with-open-rdb-raw ((db-var db-path &optional (opt (default-rocksdb-options))) &body body)
  `(let ((,db-var (open-db-raw ,db-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-close ,db-var)
       (with-errptr* (err 'rocksdb-alien-error)
         (rocksdb-options-destroy ,opt)))))

(defmacro with-rdb ((db-var db &key open close) &body body)
  "Bind DB-VAR to the database object DB for the lifetime of BODY."
  `(let ((,db-var ,db))
     ,@(when open `(open-db ,db-var))
     ,@(if close `(unwind-protect (progn ,@body) (close-db ,db-var))
           body)))

;; temp-rdb
(defvar *temp-db-path-generator*
  (lambda (&optional (name "temp-db"))
    (make-pathname :directory "tmp" :name (symbol-name (gensym name))))
  "A single arg function returning the absolute path to a temp-db path.")

(defvar *temp-db-destroy* nil)

;;; cf
(defmacro with-column ((cf-var cf) &body body)
  "Bind CF to CF-VAR for the lifetime of BODY."
  `(let ((,cf-var ,cf))
     (handler-bind ((error (lambda (condition)
                             (error 'cf-error
                                    :message
                                    (format nil "WITH-COLUMN signaled: ~A" condition)))))
       ,@body)))

(defmacro do-columns ((cf cfs) &body body)
  "Do BODY for each CF in the array CFS."
  (with-gensyms (%cf)
    `(loop for ,%cf across ,cfs
           do (with-column (,cf ,%cf) ,@body))))

;;; kv
(defmacro with-kv ((k v kv) &body body)
  `(let ((,k (kv-key ,kv))
         (,v (kv-val ,kv)))
     ,@body))

(defmacro do-kvs ((k v kvs) &body body)
  "Do BODY for each K and V in the array KVS."
  (with-gensyms (%kv)
    `(loop for ,%kv across ,kvs
           do (with-kv (,k ,v ,%kv) ,@body))))

;; TODO: sb-ext:with-current-source-form ?
;;; backup
(defmacro with-open-backup-engine-raw ((be-var be-path &optional (opt (rocksdb-options-create)))
                                       &body body)
  `(let ((,be-var (open-backup-engine-raw ,be-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-backup-engine-close ,be-var))))

;;; raw
;; Following macros introduce four anaphors - %KEY and %KLEN and if VAL is present, %VAL and %VLEN.
(defmacro with-kv-raw ((db key eptr &key (error 'kv-error) val cf) &body body)
  `(let ((%klen (length ,key))
         ,@(when val `((%vlen (length ,val)))))
     (with-errptr* (,eptr ',error :db ,db :kv ,(if val `(cons ,key ,val) key) ,@(when cf `(:cf ,cf)))
       (with-alien ((%key (* unsigned-char) (make-alien unsigned-char %klen))
                    ,@(when val `((%val (* unsigned-char) (make-alien unsigned-char %vlen)))))
         (setfa %key ,key)
         ,@(when val `((setfa %val ,val)))
         ,@body))))

(defmacro with-txn-raw ((txn eptr &key (error 'txn-error) key val cf db) &body body)
  `(let (,@(when key `((%klen (length ,key))))
         ,@(when val `((%vlen (length ,val)))))
     (with-errptr* (,eptr ',error 
                          :txn ,txn
                          ,@(when cf `(:cf ,cf))
                          ,@(when db `(:db ,db))
                          ,@(when (or key val)
                              `(:kv ,(if val `(cons ,key ,val) key))))
       (with-alien (,@(when key `((%key (* unsigned-char) (make-alien unsigned-char %klen))))
                    ,@(when val `((%val (* unsigned-char) (make-alien unsigned-char %vlen)))))
         ,@(when key `((setfa %key ,key)))
         ,@(when val `((setfa %val ,val)))
         ,@body))))


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

