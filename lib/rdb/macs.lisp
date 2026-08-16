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
                        (format *trace-output* "~&Memory fault detected: ~A~%" c)
                        (rocksdb-c-error ,e)))
                    (error (lambda (c)
                             (declare (ignore c))
                             (apply 'error ,err :message (deref (sap-alien ,e (* c-string))) ',params))))
       (progn ,@body))))

;;; rdb
;; these functions only apply to the low-level API in RDB/OBJ (structs only)
(defmacro with-open-rdb-raw ((db-var db-path &optional (opt (default-rocksdb-options))) &body body)
  `(let ((,db-var (%open-db ,db-path ,opt)))
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

(defmacro unless-null-db (slots self &body body)
  `(with-slots (db ,@slots) ,self
     (unless (null db)
       ,@body)))

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

;; TODO: sb-ext:with-current-source-form ?
;;; backup
(defmacro with-open-backup-engine-raw ((be-var be-path &optional (opt (rocksdb-options-create)))
                                       &body body)
  `(let ((,be-var (%open-backup-engine ,be-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-backup-engine-close ,be-var))))

;;; raw macros
;; Following macros introduce four anaphors - %KEY and %KLEN and if VAL is
;; present, %VAL and %VLEN. Only use these with OCTET-VECTOR and strings.

(defmacro with-kv-raw ((db key eptr &key (error 'kv-error) val cf) &body body)
  `(let ((%klen (length ,key))
         ,@(when val `((%vlen (length ,val)))))
     (with-errptr* (,eptr ',error :db ,db :kv ,(if val `(cons ,key ,val) key) ,@(when cf `(:cf ,cf)))
       (with-alien ((%key (* unsigned-char) (make-alien unsigned-char %klen))
                    ,@(when val `((%val (* unsigned-char) (make-alien unsigned-char %vlen)))))
         (setfa %key ,key)
         ,@(when val `((setfa %val ,val)))
         ,@body))))

(defmacro with-kv-raw* (key val &body body)
  `(let ((%klen (length ,key))
         ,@(when val `((%vlen (length ,val)))))
     (with-alien ((%key (* unsigned-char) (make-alien unsigned-char %klen))
                  ,@(when val `((%val (* unsigned-char) (make-alien unsigned-char %vlen)))))
       (setfa %key ,key)
       ,@(when val `((setfa %val ,val)))
       ,@body)))

(defmacro with-txn-raw ((txn eptr &key (error 'rdb-transaction-error) key val cf db) &body body)
  "Provide several bindings around BODY. TXN must be a raw transaction instance
and EPTR a symbol which is bound to an error pointer (via WITH-ERRPTR*).

When KEY is supplied, %KLEN is bound to the key length and %KEY to an
associated alien c-string. Likewise for VAL with %VLEN and %VAL."
  `(let (,@(when key `((%klen (length ,key))))
         ,@(when val `((%vlen (length ,val)))))
     (with-errptr* (,eptr ',error 
                          :txn ,txn
                          ,@(when cf `(:cf ,cf))
                          ,@(when db `(:db ,db))
                          ,@(when (or key val)
                              `(:kv ,(if val `(cons ,key ,val) key))))
       (with-alien (,@(when key `((%key c-string)))
                    ,@(when val `((%val c-string))))
         ,@(when key `((setfa %key ,key)))
         ,@(when val `((setfa %val ,val)))
         ,@body))))

;;; buffered macros
;; macros for the preferred API based on BUFFER-STREAMs.
;; TODO 2026-08-15: 
(defmacro with-ts-buf (tbuf &body body)
  "Bind a timestamp buffer to %TSLEN and %TS."
  `(let ((%tslen ,(buffer-stream-length tbuf)))
     (with-alien ((%ts (* unsigned-char) (buffer ,tbuf)))
       ,@body)))

(defmacro with-txn-buf ((txn eptr &key (error 'rdb-transaction-error) cf db key val) &body body)
  `(let (,@(when key `((%klen (buffer-stream-length ,key))
                       (%ksize (size ,key))))
         ,@(when val `((%vlen (buffer-stream-length ,val)))))
     (with-errptr* (,eptr ',error 
                          :txn ,txn
                          ,@(when cf `(:cf ,cf))
                          ,@(when db `(:db ,db))
                          ,@(when (or key val)
                              `(:kv ,(if val `(cons ,key ,val) key))))
       (with-alien (,@(when key `((%key (* unsigned-char) (buffer ,key))))
                    ,@(when val `((%val (* unsigned-char) (buffer ,val)))))
         ,@body))))

(defmacro with-kv-buf ((db kbuf vbuf eptr &key (error 'kv-error) cf) &body body)
  "binds %KSIZE %VLEN %KEY %VAL"
  `(let ((%ksize (size ,kbuf))
         (%vlen (buffer-stream-length ,vbuf)))
     (with-errptr* (,eptr ',error :db ,db :kv ,(cons kbuf vbuf) ,@(when cf `(:cf ,cf)))
       (with-alien ((%key (* unsigned-char) (buffer ,kbuf))
                    (%val (* unsigned-char) (buffer ,vbuf)))
         ,@body))))

(defmacro with-key-buf ((db kbuf eptr &key (error 'kv-error) cf) &body body)
  "binds %KSIZE %KEY"
  `(let ((%ksize (size ,kbuf)))
     (with-errptr* (,eptr ',error :db ,db :kv ,kbuf ,@(when cf `(:cf ,cf)))
       (with-alien ((%key (* unsigned-char) (buffer ,kbuf)))
         ,@body))))

;; (defmacro with-iter-buf ((iter eptr &key (error 'kv-error) cf db) &body body))

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

;;; wbwi
(defmacro with-wbwi ((var &key reserved (overwrite t) (destroy t)) &body body)
  `(let ((,var (%create-wbwi
                ,(ifret reserved 0)
                ,(ifret (and overwrite 1) 0))))
     ,@(if destroy
           `((unwind-protect (progn ,@body)
               (%destroy-wbwi ,var)))
           body)))

;;; slices
(defmacro with-slice (slice &body body)
  "Eval BODY with the rocksdb-slice pointer SLICE destructured into DATA and SIZE
values."
  `(with-alien-slots (data size) ,slice
     ,@body))

(defmacro with-pslice (slice &body body)
  "Eval BODY with the pinnable-slice pointer SLICE destructured into DATA and
SIZE values."
  `(multiple-value-bind (data size) (rocksdb::rocksdb-pinnableslice-value ,slice)
     ,@body))
