;;; rdb/macs.lisp --- macros

;;; Code:
(in-package :rdb)
;;; DB Dispatch
(defmacro define-db-surrogate (name rdb trdb)
  "Define a db-NAME surrogate function given args which dispatches to RDB or TRDB
based on the type of the car of ARGS."
  `(definline ,name (db &rest args)
     (etypecase db
       ((alien (* rocksdb)) (apply #',rdb db args))
       ((alien (* rocksdb-transactiondb)) (apply #',trdb db args)))))

;;; Base DB
(defmacro with-base-db (base-db src-db &body body)
  "Assign a symbol designated by BASE-DB to the associate base-db type of SRC-DB,
which should be a pointer to ROCKSDB-TRANSACTIONDB or
ROCKSDB-OPTIMISTICTRANSACTIONDB."
  (with-gensyms (destroy)
  `(multiple-value-bind (,base-db ,destroy) (get-base-db ,src-db)
     (unwind-protect (progn ,@body)
       (funcall ,destroy ,base-db)))))

;;; error handling
(defmacro with-errptr* ((e err &rest params) &body body)
  "Bind E to a C pointer which can be used by alien functions, and if an error is
signaled we coerce this pointer to a string and feed it to a condition of name
ERR with initargs PARAMS for the duration of BODY."
  `(with-errptr ,e
     (handler-bind ((error 
                      (lambda (c)
                        (declare (ignore c))
                        (error ,err #+nil :message #+nil (deref (cast ,e (* c-string))) ,@params))))
       ,@body)))

;;; raw

;; currently only used in the cli
(defmacro with-rdb ((db-var db &key open close destroy) &body body)
  "Bind DB-VAR to the database object DB for the lifetime of BODY."
  `(let ((,db-var ,db))
     ,@(when open `(open-db ,db-var))
     ,@(if (or close destroy) `((unwind-protect (progn ,@body) (close-db ,db-var) ,@(when destroy `((destroy-db ,db-var)))))
           body)))

(defmacro unless-null-db (slots self &body body)
  `(with-slots (db ,@slots) ,self
     (unless (null db)
       ,@body)))

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
  `(let ((%tslen (buffer-stream-length ,tbuf)))
     (with-alien ((%ts system-area-pointer (buffer ,tbuf)))
       ,@body)))

(defmacro with-ts-bufs (tbufs &body body)
  "Bind a list of timestamp buffers to %TS and %TSIZES."
  `(with-alien ((%ts (* (* unsigned-char)) (make-alien (* unsigned-char) (length ,tbufs)))
                (%tsizes (* size-t) (make-alien size-t (length ,tbufs))))
     (loop for ts in ,tbufs
           for i from 0 below (length ,tbufs)
           do (setf (deref %ts i) (sap-alien (buffer ts) (* unsigned-char))
                    (deref %tsizes i) (buffer-stream-length ts)))
       ,@body))

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
       (with-alien (,@(when key `((%key system-area-pointer (buffer ,key))))
                    ,@(when val `((%val system-area-pointer (buffer ,val)))))
         ,@body))))

(defmacro with-kv-buf ((db kbuf vbuf eptr &key (error 'kv-error) cf) &body body)
  "binds %KSIZE %VSIZE %KEY %VAL"
  `(let ((%ksize (size ,kbuf))
         (%vsize (size ,vbuf)))
     (with-errptr* (,eptr ',error :db ,db :kv (cons ,kbuf ,vbuf) ,@(when cf `(:cf ,cf)))
       (with-alien ((%key system-area-pointer (buffer ,kbuf))
                    (%val system-area-pointer (buffer ,vbuf)))
         ,@body))))

(defmacro with-kv-buf* ((db kbuf vbuf eptr &key (error 'kv-error) cf) &body body)
  "binds %KSIZE %VLEN %KEY %VAL"
  `(let ((%ksize (size ,kbuf))
         (%vlen (buffer-stream-length ,vbuf)))
     (with-errptr* (,eptr ',error :db ,db :kv (cons ,kbuf ,vbuf) ,@(when cf `(:cf ,cf)))
       (with-alien ((%key system-area-pointer (buffer ,kbuf))
                    (%val system-area-pointer (buffer ,vbuf)))
         ,@body))))

(defmacro with-key-buf ((db kbuf eptr &key (error 'kv-error) cf) &body body)
  "binds %KSIZE %KEY"
  `(let ((%ksize (size ,kbuf)))
     (with-errptr* (,eptr ',error :db ,db :kv ,kbuf ,@(when cf `(:cf ,cf)))
       (with-alien ((%key system-area-pointer (buffer ,kbuf)))
         ,@body))))

(defmacro with-kbuf ((eptr kbuf) &body body)
  "binds %KSIZE %KEY"
  `(let ((%ksize (size ,kbuf)))
     (with-errptr ,eptr
       (with-alien ((%key system-area-pointer (buffer ,kbuf)))
         ,@body))))

(defmacro with-key-range ((db sbuf ebuf eptr &key (error 'kv-error) cf) &body body)
  "binds %SSIZE %ESIZE %SKEY %EKEY"
  `(let ((%ssize (size ,sbuf))
         (%esize (size ,ebuf)))
     (with-errptr* (,eptr ',error :db ,db :kv (cons ,sbuf ,ebuf) ,@(when cf `(:cf ,cf)))
       (with-alien ((%skey system-area-pointer (buffer ,sbuf))
                    (%ekey system-area-pointer (buffer ,ebuf)))
         ,@body))))

(defmacro with-key-bufs ((kbufs eptrs) &body body)
  "binds %KLEN %KEYS %KSIZES %VALS %VSIZES. Note that errors are left unhandled."
  `(with-alien ((%keys (* (* unsigned-char)) (make-alien (* unsigned-char) (length ,kbufs)))
                (%ksizes (* size-t))
                (%klen size-t (length ,kbufs))
                (%vals (* (* unsigned-char)) (make-alien (* unsigned-char) (length ,kbufs)))
                (%vsizes (* size-t))
                (,eptrs (* c-string) (make-alien c-string (length ,kbufs))))
     (loop for k in ,kbufs
           for i from 0 below (length ,kbufs)
           do (setf (deref %keys i) (sap-alien (buffer k) (* unsigned-char))
                    (deref %ksizes i) (size k)))
     ,@body))

(defmacro with-val-bufs ((length eptrs) &body body)
  "binds %KLEN %KEYS %KSIZES %VALS %VSIZES. No errors."
  `(with-alien ((%vals (* (* rocksdb-pinnableslice)) (make-alien (* rocksdb-pinnableslice) ,length))
                (,eptrs (* c-string) (make-alien c-string ,length)))
     ,@body))

;; (defmacro with-iter-buf ((iter eptr &key (error 'kv-error) cf db) &body body))

;;; sst
(defmacro with-sst ((sst &key file comparator (destroy t)) &body body)
  "Do BODY with SST bound to a SST-FILE-WRITER. When FILE is supplied
the writer will automatically open that file.

When COMPARATOR is supplied it is used as the comparator function for
the writer. Every key inserted MUST be in ascending order, according
to the comparator. By default the ordering is binary
lexicographically.

It is up to the developer to ensure that the comparator used by a
writer is exactly the same as the comparator used when ingesting the
file by a RDB instance."
  `(let ((,sst (%sst-filewriter ,comparator)))
     ,@(when file `((%open-sst-writer ,sst ,file)))
     ,@body
     ,@(when destroy `((%destroy-sst-writer ,sst)))))

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
     (unwind-protect (progn ,@body)
       (rocksdb::rocksdb-pinnableslice-destroy ,slice))))

(defmacro with-phandle (handle &body body)
  "Eval BODY with the pinnable-handle pointer HANDLE destructured into DATA and
SIZE values."
  `(multiple-value-bind (data size) (rocksdb::rocksdb-pinnable-handle-get-value ,handle)
     (unwind-protect (progn ,@body)
       (rocksdb::rocksdb-pinnable-handle-destroy ,handle))))

