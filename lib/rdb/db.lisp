;;; rdb/db.lisp --- RDB Database API

;; RocksDB Implementation of OBJ/DB protocol.

;;; Code:
(in-package :rdb)

(defmethod make-db ((engine (eql :rocksdb)) 
                    &key
                    merge-op
                    prefix-op
                    logger
                    event-listener
                    (options (default-rocksdb-options))
                    open
                    secondary
                    path)
  (declare (ignore engine))
  (unless path (missing-argument :path))
  (when merge-op (rocksdb-options-set-merge-operator options merge-op))
  (when prefix-op (rocksdb-options-set-prefix-extractor options prefix-op))
  (when logger (rocksdb-options-set-info-log options logger))
  (when event-listener (rocksdb-options-add-eventlistener options event-listener))
  (cond
    ((and open path)
     (if secondary
         (%open-db-secondary options path secondary)
         (%open-db path options))) ; open the db
    (t (cons path options)))) ; return a cons

(defmethod make-db ((engine (eql :rocksdb-transaction))
                    &key
                    merge-op
                    prefix-op
                    logger
                    event-listener
                    (options (default-rocksdb-options))
                    (transactiondb-options (default-rocksdb-transactiondb-options))
                    open
                    path)
  (declare (ignore engine))
  (when merge-op (rocksdb-options-set-merge-operator options merge-op))
  (when prefix-op (rocksdb-options-set-prefix-extractor options prefix-op))
  (when logger (rocksdb-options-set-info-log options logger))
  (when event-listener (rocksdb-options-add-eventlistener options event-listener))
  (if open
      (%open-transactiondb options transactiondb-options path) ; open the db, OR
      (cons path options))) ; return a cons

;;; Database
(defclass rdb-object ()
  ((options :initform (default-rocksdb-options) :accessor options :initarg :options)))

(defclass rdb (database rdb-object)
  ((columns :initarg :columns :accessor columns)
   (path :initarg :path :accessor path))
  (:documentation "Standard RocksDB database wrapper.
OPTIONS is an alien ROCKSDB-OPTIONS pointer."))

(defun load-db-opts (db)
  ;; order is determined by RocksDB
  (multiple-value-bind (opts names cf-opts) (%load-opts (path db))
    (setf (columns db)
          (loop for cf-name across names
                for cf-opt across cf-opts
                ;; unless (string= *default-column-family-name* cf-name)
                collect (make-instance 'column-family :options cf-opt :name cf-name))
          (options db) opts)
    db))

(defmethod load-opts ((db rdb) &key)
  (load-db-opts db))

(defclass column-family (rdb-object database)
  ((name :initarg :name :accessor name))
  (:documentation "RocksDB Column Family.
Inherits directly from the RDB class. The DB slot is a
ROCKSDB-COLUMN-FAMILY-HANDLE."))

(defclass trdb (rdb)
  ((transactiondb-options :initform (default-rocksdb-transactiondb-options) :accessor transactiondb-options :initarg :transactiondb-options))
  (:documentation "Standard (pessimistic) Transaction DB.
TRANSACTIONDB-OPTIONS is an alien ROCKSDB-TRANSACTIONDB-OPTIONS pointer."))

(defclass otrdb (rdb) ()
  (:documentation "Optimistic Transaction DB."))

(defmethods make-db 
  (((engine (eql :rdb)) &rest initargs &key (load t) open)
   (declare (ignore engine))
   (remf initargs :open)
   (remf initargs :load)
   (let ((db (apply 'make-instance 'rdb initargs)))
     (when (and load (probe-file (path db))) (load-opts db))
     (when open (open-db db))
     db))
  (((engine (eql :trdb)) &rest initargs &key (load t) open)
   (declare (ignore engine))
   (remf initargs :open)
   (remf initargs :load)
   (let ((db (apply 'make-instance 'trdb initargs)))
     (when (and load (probe-file (path db))) (load-opts db))
     (when open (open-db db))
     db))
  (((engine (eql :otrdb)) &rest initargs &key (load t) open)
   (declare (ignore engine))
   (remf initargs :open)
   (remf initargs :load)
   (let ((db (apply 'make-instance 'otrdb initargs)))
     (when (and load (path db)) (load-opts db))
     (when open (open-db db))
     db)))

(defun repair-db (self &optional (opts (default-rocksdb-readoptions)))
  (%repair-db (path self) opts))

(defun merge-columns (self columns)
  ;; TODO 2026-08-07: using lists now, use list MERGE
  (loop for c in columns
        do (if-let ((found (find-column c self)))
             (setf (nth (columns self) (position found (columns self))) c)
             (push c (columns self)))))

(defmethod reset ((self rdb) &key (columns t) (options (default-rocksdb-options)))
  (when columns 
    (close-columns self)
    (setf (columns self) nil))
  (rocksdb-options-destroy (options self))
  (setf (options self) options)
  self)

(defun open-all-columns (self)
  "Open all columns defined in an RDB database.
This function should be used at most once for any given slot value of COLUMNS
to create them. It is an error to call this function with pre-existing
columns."
  (let ((names) (opts))
    (loop for c in (columns self)
          do (push (name c) names)
          do (push (options c) opts))
    (nreversef names)
    (nreversef opts)
    ;; ;; make sure the default column-family is opened
    ;; (unless (member *default-column-family-name* names :test 'string=)
    ;;   (push *default-column-family-name* names)
    ;;   (push (opts self) opts))
    (multiple-value-bind (db cfs)
        (%open-cfs (opts self) (name self) names opts)
      (setf (db self) db)
      ;; HACK 2026-08-18: 
      (let ((len (length names)))
        (loop for n in names
              for i below len
              for cf = (deref cfs i)
              do (setf (db (find-column (pop names) self)) cf)))
      self)))

(defmethod find-column ((cf string) (self rdb) &key)
  (find cf (columns self) :key 'name :test 'string=))

(defun open-with-columns (db)
  (if (db db)
      (cerror "Ignore and continue" 'open-db-error 
              :db db
              :message "Database is already open")
      (let* ((cols (columns db))
             (ncols (length cols))
             (names (loop for c in cols collect (name c)))
             (opts (loop for c in cols collect (options c))))
        (multiple-value-bind (db-sap cfs) (%open-cfs (opts db) (path db) names opts)
          (setf (db db) db-sap)
          (loop for i below ncols
                for c in (columns db)
                for cf = (deref cfs i)
                do (setf (db c) cf))
          ;; this function should never return a value.
          (values)))))

(defun close-columns (db)
  (loop for cf in (columns db)
        ;; unless (string= (name cf) *default-column-family-name*)
        do (close-db cf)))

;; TODO 2026-08-20: unless-key-exists-p
(defmethods insert-key
  (((self rdb) (key string) (val string) &key column)
   (insert-key self (string-to-octets key) (string-to-octets val) :column column))
  (((self rdb) (key string) val &key column)
   (insert-key self (string-to-octets key) val :column column))
  (((self rdb) key (val string) &key column)
   (insert-key self key (string-to-octets val) :column column)))

(defmethods get-val 
  (((self rdb) (key string) &key (opts (rocksdb-readoptions-create)) column pin)
   (unless-null-db () self
     (if column
         (%get-cf-str db (db (find-column column self)) key opts pin)
         (%get-kv-str db key opts pin))))
  (((self rdb) key &key (opts (rocksdb-readoptions-create)) column pin)
   (unless-null-db () self
     (if column
         (%get-cf db (db (find-column column self)) key opts pin)
         (%get-kv db key opts pin))))
  (((self trdb) key &key (opts (rocksdb-readoptions-create)) column pin)
   (unless-null-db () self
     (if column
         (%transactiondb-get-cf db (db (find-column column self)) key opts pin)
         (%transactiondb-get-kv db key opts pin))))
  (((self trdb) (key string) &key (opts (rocksdb-readoptions-create)) column pin)
   (unless-null-db () self
     (if column
         (%transactiondb-get-cf-str db (db (find-column column self)) key opts pin)
         (%transactiondb-get-kv-str db key opts pin)))))

(defmethod multi-get ((self rdb) keys &key (opts (rocksdb-readoptions-create)) columns)
  (if columns
      (%multi-get-cf-kv (db self) (mapcar 'db columns) keys opts)
      (%multi-get-kv (db self) keys opts)))

(defmethod make-column ((db rdb) &rest args)
  (let ((col (apply 'make-instance 'column-family args)))
    (setf (db col) (if (equal (name col) *default-column-family-name*)
                       (rocksdb-get-default-column-family-handle (db db))
                       (%create-cf (db db) (name col) (options col))))
    (push col (columns db))
    col))

(defmethod make-column ((db trdb) &rest args)
  (let ((col (apply 'make-instance 'column-family args)))
    (setf (db col) (if (equal (name col) *default-column-family-name*)
                       (rocksdb-get-default-column-family-handle (db db))
                       (%txn-create-cf (db db) (name col) (options col))))
    (push col (columns db))
    col))

(defmethod (setf find-column) ((new column-family) (cf string) (self rdb) &key)
  "Find and replace a column by name."
  (nsubstitute new (find-column cf self) (columns self)))

(defaccessor name ((self rdb)) (path self))
(defaccessor sap ((self rdb)) (db self))
(defaccessor opts ((self rdb) &key) (options self))
;; TODO
(defaccessor opt ((self rdb) key) (opt (opts self) key))
(defmethods prop 
  (((self rdb) (name string))
   (unless-null-db () self
     (rocksdb-property-value db name)))
  (((self rdb) (name symbol))
   (prop self (string-downcase (concatenate 'string "rocksdb." (symbol-name name)))))
  (((self trdb) (name string))
   (unless-null-db () self
     (rocksdb-property-value (rocksdb-transactiondb-get-base-db db) name)))
  (((self otrdb) (name string))
   (unless-null-db () self
     (rocksdb-property-value (rocksdb-optimistictransactiondb-get-base-db db) name))))

(defmethod ingest-db ((self rdb) (files list) &key column (options (rocksdb-ingestexternalfileoptions-create)))
  (if column
      (%ingest-db-cf (db self) (db column) files options)
      (%ingest-db (db self) files options)))

(defmethod derive-schema ((self rdb))
  (apply 'make-schema
         (loop for c in (columns self)
               collect (field-from-cf (db c)))))

(defmethod open-db ((self rdb))
  (with-slots (path db options) self
    (if db
        (progn
          (cerror "Ignore and continue" 'open-db-error 
                  :db db
                  :message "Database is already open")
          db)
        ;; if the db path exists we assume the db was created with :load t
        ;; (LOAD-OPTS)
        (if (probe-file path)
            (open-with-columns self)
            (setf db (%open-db path options))))))

(defmethod open-db ((self trdb))
  (with-slots (path db options transactiondb-options) self
    (if db
        (progn
          (cerror "Ignore and continue" 'open-db-error
                  :db db
                  :message "Database is already open")
          db)
        (if (probe-file path)
            (open-with-columns self)
            (setf db (%open-transactiondb options transactiondb-options (namestring path)))))))

(defmethod open-db ((self otrdb))
  (with-slots (path db options) self
    (if db
        (progn
          (cerror "Ignore and continue" 'open-db-error
                  :db db
                  :message "Database is already open")
          db)
        (if (probe-file path)
            (open-with-columns self)
            (setf db (%open-optimistictransactiondb options (namestring path)))))))

(defun open-backup-db (self &key path) ;; opts env
  (%open-backup-engine (options self) path))

(defun open-secondary-db (self &key path (options (default-rocksdb-options)))
  (%open-db-secondary options (path self) path))

(defmethod checkpoint ((self rdb) &key path log-size-for-flush)
  (unless-null-db () self
    (let ((chk (%make-checkpoint db)))
      (%create-checkpoint chk path log-size-for-flush))))

(defmethod snapshot ((self rdb) &key)
  (unless-null-db () self
    (%create-snapshot db)))

(defmethod restore ((self rdb) (from string) &key id options)
  (unless-null-db (path) self
    (%restore-from-backup (backup self :path from) path from id options)))

(defmethod backup ((self rdb) &key path)
  (unless-null-db (options) self
    (if (null path)
        (error 'open-backup-engine-error :db db
                                         :message "PATH must not be nil when no backups exist")
        (%create-new-backup (open-backup-db self :path path) db))))

(defmethod flush ((self rdb) &key wait)
  (%flush-db (db self) wait))
;; (defmethod close-db :before ((self srdb) &key)
;;   (close-columns self))
(defmethod close-db ((self rdb) &key reset) 
  (when (and reset (options self)) (setf (options self) (rocksdb-options-destroy (options self))))
  (unless-null-db () self
    (setf (db self) (rocksdb-close (db self)))))
(defmethod close-db ((self trdb) &key reset)
  (when (and reset #1=(transactiondb-options self))
    (setf (transactiondb-options self)
          (rocksdb-transactiondb-options-destroy #1#)))
  (unless-null-db () self
    (setf (db self) (rocksdb-transactiondb-close db))))
(defmethod close-db ((self otrdb) &key)
  (unless-null-db () self
    (setf (db self) (rocksdb-optimistictransactiondb-close db))))
(defmethod print-object ((self rdb) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":open ~A" (db-open-p self))))

(defmethod db-open-p ((self rdb))
  (with-slots (db) self
    (and db (typep db 'alien) (not (null db)))))

(defmethod db-closed-p ((self rdb))
  (null (db self)))

(defmethod destroy-db ((self rdb))
  ;; close all handles before destruction ensues
  (close-db self)
  (%destroy-db (namestring (path self))))

(defmethod close-backup ((self rdb))
  (with-slots (backup) self
    (unless (null backup)
      (setf backup (%close-backup-engine backup)))))

(defmethod shutdown-db :before ((self database) &key)
  (log:trace! "shutting down database" (path self)))

(defmethod shutdown-db ((self rdb) &key wait cancel)
  (unless-null-db (options) self
    (cond 
      ((and wait (not cancel))
       (let ((opts (if (eq t wait) (default-rocksdb-wait-for-compact-options) wait)))
         (with-errptr e (rocksdb-wait-for-compact db opts e)
           (rocksdb-wait-for-compact-options-destroy opts)
           (setf (db self) (free-alien (db self))))))
      (t (when cancel (rocksdb-cancel-all-background-work db wait))
         (close-db self)))
    ;; HACK 2026-08-18: 
    (when (slot-boundp! self 'columns)
      (mapcar (lambda (x) (setf (db x) nil
                                (options x) nil))
              (columns self)))
    (setf options nil)))

(defmethod shutdown-db :around ((self trdb) &key wait)
  (with-slots (transactiondb-options) self
    (when transactiondb-options 
      (setf (transactiondb-options self) (rocksdb-transactiondb-options-destroy transactiondb-options))))
  (call-next-method self :wait wait))

(defmethod get-value (elt (self rdb))
  (%get-kv (db self) elt (default-rocksdb-readoptions)))

(defmethod get-value (elt (self trdb))
  (%transactiondb-get-kv (db self) elt (default-rocksdb-readoptions)))

(defmethods put-key 
  (((self rdb) (key t) (val t) &key)
   (%put-kv
    (db self)
    key
    val))
  (((self rdb) (key string) (val string) &key)
   (%put-kv
    (db self)
    (sb-ext:string-to-octets key)
    (sb-ext:string-to-octets val))))

(defmethod delete-key ((self rdb) key &key (opts (default-rocksdb-writeoptions)))
  (%delete-kv (db self) key opts))

(defmethod merge-key ((self rdb) key val &key (opts (rocksdb-writeoptions-create)) column)
  (if column
      (%merge-cf (db self) (find-column column self) key val opts)
      (%merge-kv (db self) key val opts)))

(defmethod merge-key ((self rdb) (key string) (val string) &key (opts (rocksdb-writeoptions-create)) column)
  (if column
      (%merge-cf-str (db self) (find-column column self) key val opts)
      (%merge-kv-str (db self) key val opts)))

;;; Columns
(defaccessor sap ((self column-family)) (db self))

(defmethod free ((self column-family))
  (setf (db self) (%destroy-cf (db self))))

(defmethod close-db ((self column-family) &key)
  (unless-null-db (options) self
    (setf options (rocksdb-options-destroy options))
    (free self)))

;;; SST File Writer
(defstruct sst-file-writer
  (path nil :type (or null pathname string))
  (sap (%sst-filewriter) :type (alien (* rocksdb-sstfilewriter))))

(defaccessor sap ((self sst-file-writer)) (sst-file-writer-sap self))
(defaccessor path ((self sst-file-writer)) (sst-file-writer-path self))

(defmethod size ((self sst-file-writer)) (%sst-file-size (sst-file-writer-sap self)))

(defmethod open-db ((self sst-file-writer))
  (%open-sst-writer (sst-file-writer-sap self) (namestring (sst-file-writer-path self))))

(defmethod close-db ((self sst-file-writer) &key)
  (%finish-sst-writer (sst-file-writer-sap self)))

(defmethod free ((self sst-file-writer))
  (with-slots (sap) self
    (unless (null sap)
      (setf (sap self) (%destroy-sst-writer sap)))))

(defmethod shutdown-db ((self sst-file-writer) &key)
  (free self))

(defmethod print-object ((self sst-file-writer) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":path ~A ~@{:size ~A~}" (sst-file-writer-path self)
            (when (sst-file-writer-sap self) (size self)))))

(defmethod put-key ((self sst-file-writer) key val &key timestamp)
  (if timestamp
      (%sst-put-ts (sst-file-writer-sap self) key val timestamp)
      (%sst-put (sst-file-writer-sap self) key val)))

(defmethod put-key ((self sst-file-writer) (key simple-string) (val simple-string) &key timestamp)
  (if timestamp
      (%sst-put-ts (sst-file-writer-sap self) key val timestamp)
      (%sst-put-str (sst-file-writer-sap self) key val)))

(defmethod delete-key ((self sst-file-writer) key &key timestamp start end)
  (cond 
    (timestamp (%sst-delete-ts (sst-file-writer-sap self) key timestamp))
    ((or start end) (%sst-delete-range (sst-file-writer-sap self) start end))
    (t (%sst-delete (sst-file-writer-sap self) key))))
