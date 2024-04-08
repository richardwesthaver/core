(in-package :rdb)

;;; rdb-opts
(defvar *rdb-opts-lookup-table*
  (let ((table (make-hash-table :test #'equal)))
    (mapc (lambda (x) (setf (gethash (car x) table) (cdr x)))
          (loop for y across *rocksdb-options*
                collect (cons y (format nil "~:@(rocksdb-options-set-~x~)" y))))
    table))

(defmacro rdb-opt-setter (key)
  `(find-symbol (format nil "~:@(rocksdb-options-set-~x~)" ,key) :rocksdb))

(defmacro rdb-opt-getter (key)
  `(find-symbol (format nil "~:@(rocksdb-options-get-~x~)" ,key) :rocksdb))

(defun %set-rocksdb-option (opt key val)
  (funcall (rdb-opt-setter key) opt val))

#| special cases
WARNING: #<OPT-HANDLER-MISSING compression-options {101A423693}>
WARNING: #<OPT-HANDLER-MISSING allow-mmap-write {101A5F0C93}>
WARNING: #<OPT-HANDLER-MISSING use-direct-io-for-flush-compaction {101A5F1913}>
WARNING: #<OPT-HANDLER-MISSING stas-persist-period-sec {101A5F32C3}>
WARNING: #<OPT-HANDLER-MISSING writable-file-max-buffer-size {101A5F4523}>
WARNING: #<OPT-HANDLER-MISSING disable-auto-compactions {101A5F54E3}>
WARNING: #<OPT-HANDLER-MISSING prepare-for-bulk-load {101A5F62E3}>
WARNING: #<OPT-HANDLER-MISSING memtable-vector-rep {101A5F6DB3}>
WARNING: #<OPT-HANDLER-MISSING memtable-prefix-bloom-size-ratio {101A5F78B3}>
WARNING: #<OPT-HANDLER-MISSING hash-skip-list-rep {101A620573}>
WARNING: #<OPT-HANDLER-MISSING plain-table-factory {101A621083}>
WARNING: #<OPT-HANDLER-MISSING min-level-to-compress {101A621B53}>
WARNING: #<OPT-HANDLER-MISSING inplace-update-num-locks {101A6230F3}>
WARNING: #<OPT-HANDLER-MISSING universal-compaction-options {101A624CD3}>
WARNING: #<OPT-HANDLER-MISSING ratelimiter {101A625723}>
WARNING: #<OPT-HANDLER-MISSING row-cache {101A6262E3}>
|#

(defun %get-rocksdb-option (opt key)
  (if-let ((g (rdb-opt-getter key)))
    (funcall g opt)
    (warn 'opt-handler-missing :message key)))

(defclass rdb-opts ()
  ((table :initarg :table :type hash-table :accessor rdb-opts-table)
   (sap :initarg :sap :type (or null alien) :accessor rdb-opts-sap)))

(defmethod initialize-instance ((self rdb-opts) &rest initargs &key &allow-other-keys)
  (with-slots (sap table) self
    (unless (getf initargs :table) (setf table (make-hash-table :test #'equal)))
    (unless (getf initargs :sap) (setf sap (rocksdb-options-create)))
    (loop for (k v) on initargs by #'cddr while v
          do (let ((k (typecase k
                        (string (string-downcase k))
                        (symbol (string-downcase (symbol-name k)))
                        (t (string-downcase (format nil "~s" k))))))
               (set-opt self k v)))
    self))

(defun make-rdb-opts (&rest values)
  (let ((opts (apply #'make-instance 'rdb-opts values)))
    (push-sap* opts)
    opts))

(defmethod get-opt ((self rdb-opts) key)
  "Return the current value of KEY in SELF if found, else return nil."
  (gethash key (rdb-opts-table self)))

(defmethod set-opt ((self rdb-opts) key val &key push)
  "Set the VAL of KEY in SELF with '(setf (gethash SELF KEY) VAL)'."
  (prog1
      (setf (gethash key (rdb-opts-table self)) val)
    (when push (push-sap self key))))

(defmethod push-sap ((self rdb-opts) key)
  "Push KEY from slot :TABLE to the instance :SAP."
  (%set-rocksdb-option (rdb-opts-sap self) key (get-opt self key)))

(defmethod push-sap* ((self rdb-opts))
  "Initialized the SAP slot with values from TABLE."
  (with-slots (table) self
    (loop for k in (hash-table-keys table)
          do (push-sap self k))))

(defmethod pull-sap ((self rdb-opts) key)
  (setf (gethash key (rdb-opts-table self)) (%get-rocksdb-option (rdb-opts-sap self) key)))

(defmethod pull-sap* ((self rdb-opts))
  (with-slots (table) self
    (loop for k in (hash-table-keys table)
          do (pull-sap self k))
    table))

(defmethod backfill-opts ((self rdb-opts) &key full)
  "Backfill the TABLE slot with values from SAP.

When FULL is non-nil, retrieve the full set of options available, not
just the keys currently present in TABLE."
  (if full
      (loop for k across *rocksdb-options*
            do (pull-sap self k))
      (pull-sap* self))
  (rdb-opts-table self))
    
(defun default-rdb-opts () 
  ;; TODO 2024-03-10: handle lisp->C types
  (make-rdb-opts :create-if-missing 1))

(defclass rdb-kv ()
  ((key :initarg :key :type octet-vector :accessor rdb-key)
   (val :initarg :val :type octet-vector :accessor rdb-val)))

(defmethod make-kv (key val)
  (make-instance 'rdb-kv 
    :key (make-key key) 
    :val (make-val val)))

(defvar *default-rdb-kv* (make-kv #() #()))

;;; column family
(defstruct (rdb-cf (:constructor make-rdb-cf (name &key kv sap)))
  "RDB Column Family structure. Contains a name, a cons of (rdb-key-type
. rdb-val-type), and a system-area-pointer to the underlying
rocksdb_cf_t handle."
  (name "" :type string)
  (kv *default-rdb-kv* :type rdb-kv)
  (sap nil :type (or null alien)))

;;; rdb
(defstruct (rdb (:constructor make-rdb (name opts &optional cfs db)))
  (name "" :type string)
  (opts (default-rdb-opts) :type rdb-opts)
  (cfs (make-array 0 :element-type 'rdb-cf :adjustable t :fill-pointer 0) :type (array rdb-cf))
  (db nil :type (or null alien)))

;; (defvar *default-rdb-opts* (default-rdb-opts))

(defmethod print-object ((self rdb) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":cfs ~A" (length (rdb-cfs self)))))
  
(defun create-db (name &key opts cfs open)
  "Construct a new RDB instance from NAME.

OPTS = rdb-opts
CFS = (sequence rdb-cf)
OPEN = boolean

When OPEN is non-nil, the database and all column families are opened
and internal sap slots are initialized."
  (when (probe-file name) (log:warn! "directory already exists: " name))
  (let* ((opts (or opts (default-rdb-opts)))
         (obj
           (make-rdb (string-right-trim '(#\/)
                                        (typecase name
                                          (pathname (namestring name))
                                          (string name)
                                          (t (error "invalid NAME: ~S" name))))
                     opts
                     (or (when cfs
                           (typecase cfs
                             (list (coerce cfs 'vector))
                             (vector cfs)
                             (rdb-cf (vector cfs))
                             (t (log:warn! "invalid CF passed to create-db"))))
                         (make-array 0 :element-type 'rdb-cf :fill-pointer 0)))))
    (when open
      (open-db obj)
      (create-cfs obj))
    obj))

(defmethod push-cf ((cf rdb-cf) (db rdb))
  (vector-push cf (rdb-cfs db)))

;; TODO: fix
(defmethod create-cf ((db rdb) (cf rdb-cf))
  (setf (rdb-cf-sap cf)
        (create-cf-raw (rdb-db db) (rdb-cf-name cf))))

(defmethod close-cf ((cf rdb-cf))
  (with-slots (sap) cf
    (unless (null sap)
      (free-alien sap))))

(defmethod open-db ((self rdb))
  (with-slots (name db opts) self
    (setq db (open-db-raw name (rdb-opts-sap opts)))))

(defmethod create-cfs ((self rdb) &key &allow-other-keys)
  (loop for cf across (rdb-cfs self)
        do (create-cf self cf)))

(defmethod close-cfs ((self rdb) &key &allow-other-keys)
  (with-slots (cfs) self
    (declare (type (array rdb-cf) cfs))
    (loop for cf across cfs
          do (setf cf (close-cf cf)))))

(defmethod close-db ((self rdb) &key &allow-other-keys)
  (with-slots (db cfs) self
    (unless (null db)
      (close-cfs self)
      (setf db (close-db-raw db)))))

(defmethod destroy-db ((self rdb))
  (destroy-db-raw (rdb-name self)))

(defmethod put-key ((self rdb) key val)
  (put-kv-raw
   (rdb-db self)
   key 
   val))

(defmethod put-kv ((self rdb) (kv rdb-kv))
  (put-kv-raw
   (rdb-db self)
   (rdb-key kv)
   (rdb-val kv)))

(defmethod insert-key ((self rdb) key val &key cf)
  (if cf
      (put-cf-raw
       (rdb-db self)
       (rdb-cf-sap (find cf (rdb-cfs self) :key #'rdb-cf-name :test #'equal))
       key
       val)
      (put-key self key val)))

(defmethod insert-key ((self rdb) (key string) (val string) &key cf)
  (insert-key self (string-to-octets key) (string-to-octets val) :cf cf))

(defmethod insert-key ((self rdb) (key string) val &key cf)
  (insert-key self (string-to-octets key) val :cf cf))

(defmethod insert-key ((self rdb) key (val string) &key cf)
  (insert-key self key (string-to-octets val) :cf cf))

(defmethod insert-kv ((self rdb) (kv rdb-kv) &key cf)
  (if cf
      (put-cf-raw (rdb-db self)
                  (rdb-cf-sap
                   (find cf (rdb-cfs self)
                         :key #'rdb-cf-name
                         :test #'string=))
                  (rdb-key kv)
                  (rdb-val kv))
      (put-kv self kv)))

(defmethod get-key ((self rdb) (key string) &key (opts (rocksdb-readoptions-create)) cf)
  (with-slots (db) self
    (when cf
      (get-cf-str-raw db cf key opts)
      (get-kv-str-raw db key opts))))
