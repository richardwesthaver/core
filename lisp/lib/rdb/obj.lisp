(in-package :rdb)

;;; rdb-opts
(defvar *rdb-opts-lookup-table*
  (let ((table (make-hash-table :test #'equal)))
    (mapc (lambda (x) (setf (gethash (car x) table) (cdr x)))
          (loop for y across *rocksdb-options*
                collect (cons y (format nil "~:@(rocksdb-options-set-~x~)" y))))
    table))

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

;;; iterator
(defstruct (rdb-iter (:constructor make-rdb-iter (&optional sap)))
  (sap nil :type (or null alien)))

;;; column family
(defstruct (rdb-cf (:constructor make-rdb-cf (name &key kv sap)))
  "RDB Column Family structure. Contains a name, a cons of (rdb-key-type
. rdb-val-type), and a system-area-pointer to the underlying
rocksdb_cf_t handle."
  (name "" :type string)
  (kv *default-rdb-kv* :type rdb-kv)
  (sap nil :type (or null alien)))

;;; rdb-stats
(defstruct (rdb-stats (:constructor make-rdb-stats (&optional sap)))
  (sap nil :type (or null alien)))

;;; metadata
(defstruct rdb-cf-metadata
  (name "default" :type string)
  (size 0 :type fixnum)
  (level-count 7 :type fixnum)
  (file-count 0 :type fixnum)
  (sap nil :type (or null alien)))

(defmethod get-metadata ((self rdb-cf-metadata) &optional (level 0))
  (with-slots (sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (make-rdb-level-metadata :sap (rocksdb-column-family-metadata-get-level-metadata sap level)))))

(defmethod print-object ((self rdb-cf-metadata) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (name size level-count file-count) self
      (format stream "~A :size ~A :levels ~A :files ~A" name size level-count file-count))))
  
(defmethod pull-sap* ((self rdb-cf-metadata))
  (with-slots (name size level-count file-count sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (setf name (rocksdb-column-family-metadata-get-name sap)
              size (rocksdb-column-family-metadata-get-size sap)
              level-count (rocksdb-column-family-metadata-get-level-count sap)
              file-count (rocksdb-column-family-metadata-get-file-count sap)))
    self))

(defstruct rdb-level-metadata
  (level 0 :type fixnum)
  (size 0 :type fixnum)
  (file-count 0 :type fixnum)
  (sap nil :type (or null alien)))

(defmethod get-metadata ((self rdb-level-metadata) &optional (file 0))
  (with-slots (sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (make-rdb-sst-file-metadata :sap (rocksdb-level-metadata-get-sst-file-metadata sap file)))))

(defmethod print-object ((self rdb-level-metadata) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (level size file-count) self
      (format stream "~A :size ~A :files ~A" level size file-count))))

(defmethod pull-sap* ((self rdb-level-metadata))
  (with-slots (level size file-count sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (setf level (rocksdb-level-metadata-get-level sap)
              size (rocksdb-level-metadata-get-size sap)
              file-count (rocksdb-level-metadata-get-file-count sap)))
    self))

;; NOTE: we only store the sizes of largest and smallest key, not the
;; keys themselves. This may change in the future.
(defstruct rdb-sst-file-metadata
  (relative-filename "" :type string)
  (directory "" :type string)
  (size 0 :type fixnum)
  (smallestkey 0 :type fixnum)
  (largestkey 0 :type fixnum)
  (sap nil :type (or null alien)))

(defmethod print-object ((self rdb-sst-file-metadata) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (relative-filename directory size smallestkey largestkey) self
      (format stream "~A :dir ~A :size ~A :smallest ~A :largest ~A"
              relative-filename directory size smallestkey largestkey))))

(defmethod pull-sap* ((self rdb-sst-file-metadata))
  (with-slots (relative-filename directory size smallestkey largestkey sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (with-alien ((ssize size-t 0)
                     (lsize size-t 0))
          (rocksdb-sst-file-metadata-get-largestkey sap (addr lsize))
          (rocksdb-sst-file-metadata-get-smallestkey sap (addr ssize))
          (setf relative-filename (rocksdb-sst-file-metadata-get-relative-filename sap)
                directory (rocksdb-sst-file-metadata-get-directory sap)
                size (rocksdb-sst-file-metadata-get-size sap)
                largestkey lsize
                smallestkey ssize)))
    self))

;;; rdb
(defstruct (rdb (:constructor make-rdb (name opts &optional cfs db)))
  (name "" :type string)
  (opts (default-rdb-opts) :type rdb-opts)
  (cfs (make-array 0 :element-type 'rdb-cf :adjustable t :fill-pointer 0) :type (array rdb-cf))
  (db nil :type (or null alien))
  (backup nil :type (or null alien))
  (snapshots #() :type (array alien)))

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
                             ((array rdb-cf) cfs)
                             (rdb-cf (vector cfs))
                             (t (log:warn! "invalid CF passed to create-db"))))
                         (make-array 0 :element-type 'rdb-cf :fill-pointer 0)))))
    (when open
      (open-db obj))
    obj))

(defmethod push-cf ((cf rdb-cf) (db rdb))
  (vector-push cf (rdb-cfs db)))

;; TODO: fix
(defmethod create-cf ((db rdb) (cf rdb-cf))
  (setf (rdb-cf-sap cf)
        (create-cf-raw (rdb-db db) (rdb-cf-name cf) (rdb-opts-sap (rdb-opts db)))))

(defmacro unless-null-db (slots self &body body)
  `(with-slots (db ,@slots) ,self
     (unless (null db)
       ,@body)))

(defmethod destroy-cf ((cf rdb-cf))
  (with-slots (sap) cf
    (unless (null sap)
      (setf sap (destroy-cf-raw sap)))))

(defmethod set-opt ((self rdb) key val &key push)
  (with-slots (opts) self
    (set-opt opts key val :push push)))

(defmethod get-opt ((self rdb) key)
  (with-slots (opts) self
    (get-opt opts key)))

(defmethod push-opts ((self rdb))
  (with-slots (opts) self
      (push-sap* opts)))

(defmethod open-db ((self rdb))
  (with-slots (name db opts) self
    (if db
        (rdb-error "DB already opened - close before re-opening")
        (setf db (open-db-raw name (rdb-opts-sap opts))))))

(defmethod get-prop ((self rdb) (propname string))
  (unless-null-db () self
    (get-property-raw db propname)))

(defmethod repair-db ((self rdb) &key)
  (repair-db-raw (rdb-name self)))

(defmethod open-backup-db ((self rdb) &key path)
  (with-slots (opts) self
    (setf (rdb-backup self) (open-backup-engine-raw path (rdb-opts-sap opts)))))

(defmethod close-backup-db ((self rdb))
  (with-slots (backup) self
    (unless (null backup)
      (close-backup-engine-raw backup))))

(defmethod backup-db ((self rdb) &key path)
  (unless-null-db (opts backup) self
    (when (null backup)
      (if (null path)
          (error 'open-backup-engine-error :db db)
          (open-backup-db self :path path)))
    (create-new-backup-raw backup db)))

(defmethod restore-db ((self rdb) (from string) &key id opts)
  (unless-null-db (name backup) self
    (when (null backup)
      (open-backup-db self :path from))
    (restore-from-backup-raw backup name from id opts)))

(defmethod snapshot-db ((self rdb))
  (unless-null-db (snapshots) self
    (vector-push-extend (create-snapshot-raw db) snapshots)))

(defmethod get-metadata ((self rdb) &optional cf)
  (make-rdb-cf-metadata :sap (get-metadata-raw (rdb-db self) cf)))

(defmethod get-stats ((self rdb) &optional (htype (rocksdb-statistics-level "all")))
  (make-rdb-stats (get-stats-raw (rdb-opts-sap (rdb-opts self)) htype)))

(defmethod create-iter ((self rdb) &optional cf (opts (rocksdb-readoptions-create)))
  (unless-null-db () self
    (make-rdb-iter (if cf
                       (create-cf-iter-raw db cf opts)
                       (create-iter-raw db opts)))))

(defmethod print-stats ((self rdb) &optional stream)
  (print (rocksdb-options-statistics-get-string (rdb-opts-sap (rdb-opts self))) stream))

(defmethod flush-db ((self rdb) &key) ;; todo flushopts
  (flush-db-raw (rdb-db self)))

(defmethod sync-db ((self rdb) (other null) &key)
  (flush-db self))

(defmethod shutdown-db ((self rdb) &key wait)
  (log:debug! "shutting down database" (rdb-name self))
  (when-let ((db (rdb-db self)))
    (rocksdb-cancel-all-background-work db wait)
    (close-db self)))

(defmethod create-cfs ((self rdb) &key &allow-other-keys)
  (if (null (rdb-db self))
      (warn 'db-missing :message "ignoring attempt to create column-families before opening")
      (loop for cf across (rdb-cfs self)
            do (create-cf self cf))))

(defmethod destroy-cfs ((self rdb) &key &allow-other-keys)
  (with-slots (cfs) self
    (declare (type (array rdb-cf) cfs))
    (loop for cf across cfs
          do (setf cf (destroy-cf cf)))))

(defmethod close-db ((self rdb) &key &allow-other-keys)
  (with-slots (db cfs backup snapshots) self
    (close-backup-db self)
    (unless (zerop (length snapshots))
      (loop for s across snapshots do (release-snapshot-raw db s)))
    (destroy-cfs self)
    (unless (null db)
      (close-db-raw db))))

(defmethod destroy-db ((self rdb))
  ;; close all handles before destruction ensues
  (close-db self)
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
    (if cf
        (get-cf-str-raw db cf key opts)
        (get-kv-str-raw db key opts))))

(defmethod get-key ((self rdb) key &key (opts (rocksdb-readoptions-create)) cf)
  (with-slots (db) self
    (if cf
        (get-cf-raw db cf key opts)
        (get-kv-raw db key opts))))
