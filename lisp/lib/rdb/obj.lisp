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

(defun %get-rocksdb-option (opt key)
  (if-let ((g (rdb-opt-getter key)))
    (funcall g opt)
    (warn 'opt-handler-missing :message key)))

(defun opt-no-setter-p (k)
  (let ((k (typecase k
             (string (string-downcase k))
             (symbol (string-downcase (symbol-name k)))
             (t (string-downcase (format nil "~s" k))))))
    (member t
            (mapcar (lambda (x) (equal k x)) (list "parallelism" "enable-statistics")))))

(defclass rdb-opts ()
  ((table :initarg :table :type hash-table :accessor rdb-opts-table)
   (sap :initform nil :initarg :sap :type (or null alien) :accessor rdb-opts-sap)))

(defmethod initialize-instance ((self rdb-opts) &rest initargs &key &allow-other-keys)
  (with-slots ((%sap sap) (%table table)) self
    ;; initialize slots - remember, initargs doesn't refer to slot
    ;; names, they're opt names.
    (setf %table (or (cdr (remprop 'initargs :table)) (make-hash-table :test 'equal))
          %sap (or (cdr (remprop 'initargs :sap)) (rocksdb-options-create)))
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

(defun make-rdb-opts* (alien)
  "Coerce ALIEN into an RDB-OPTS struct. This function doesn't populate the
values in Lisp, just binds the sap."
  (make-instance 'rdb-opts :sap alien))

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
          ;; note how we don't handle any special cases here - we can
          ;; always set an opt but sometimes we can't get it.
          do (push-sap self k))))

(defmethod pull-sap ((self rdb-opts) key)
  (setf (gethash key (rdb-opts-table self)) (%get-rocksdb-option (rdb-opts-sap self) key)))

(defmethod pull-sap* ((self rdb-opts))
  (with-slots (table) self
    (loop for k in (hash-table-keys table)
          unless (opt-no-setter-p k)
          do (pull-sap self k))
    table))

(defmethod backfill-opts ((self rdb-opts) &key full)
  "Backfill the TABLE slot with values from SAP.

When FULL is non-nil, retrieve the full set of options available, not
just the keys currently present in TABLE."
  (if full
      (loop for k across *rocksdb-options*
            unless (opt-no-setter-p k)
            do (pull-sap self k))
      (pull-sap* self))
  (rdb-opts-table self))

(defun default-rdb-opts () 
  (make-rdb-opts :create-if-missing t :create-missing-column-families t
                 :parallelism (num-cpus)))

(defvar *default-kv* (make-kv))

;;; iterator
(defclass rdb-iter (sequence)
  ((sap :initform nil :initarg :sap :type (or null alien) :accessor rdb-iter-sap)))

(defmethod iter-valid-p ((self rdb-iter))
  (rocksdb-iter-valid (rdb-iter-sap self)))

(defmethod iter-seek-to-first ((self rdb-iter))
  (rocksdb-iter-seek-to-first (rdb-iter-sap self))) 

(defmethod iter-seek-to-last ((self rdb-iter))
  (rocksdb-iter-seek-to-last (rdb-iter-sap self)))

(defmethod iter-seek-for-prev ((self rdb-iter) (key vector) &key)
  (rocksdb-iter-seek-for-prev (rdb-iter-sap self) key (length key)))

(defmethod iter-seek ((self rdb-iter) (key simple-vector) &key)
  (rocksdb-iter-seek (rdb-iter-sap self) key (length key)))

(defmethod iter-next ((self rdb-iter))
  (rocksdb-iter-next (rdb-iter-sap self)))

(defmethod iter-prev ((self rdb-iter))
  (rocksdb-iter-prev (rdb-iter-sap self)))

(defmethod iter-key ((self rdb-iter))
  (with-alien ((klen size-t))
    (let ((key (rocksdb-iter-key (rdb-iter-sap self) (addr klen))))
      (let ((k (make-array klen :element-type 'octet)))
        (clone-octets-from-alien key k klen)
        (values
         k
         klen)))))

(defmethod iter-val ((self rdb-iter))
  (with-alien ((vlen size-t))     
    (let ((val (rocksdb-iter-value (rdb-iter-sap self) (addr vlen))))
      (let ((v (make-array vlen :element-type 'octet)))
        (clone-octets-from-alien val v vlen)
        (values
         v
         vlen)))))

(defmethod iter-kv ((self rdb-iter))
  (make-kv (iter-key self) (iter-val self)))

(defmethod iter-timestamp ((self rdb-iter))
  (with-alien ((tslen size-t))
    (values
     (rocksdb-iter-timestamp (rdb-iter-sap self) (addr tslen))
     tslen)))

;;; column family
(defstruct (rdb-cf (:constructor make-rdb-cf (name &key opts key-type val-type sap)))
  "RDB Column Family structure. Contains a name, key-type, val-type,
and a system-area-pointer to the underlying rocksdb_cf_t handle.

A NIL key-type or val-type indicates an unitialized value which defaults to
'octet-vector. This is needed to distinguish the value 'octet-vector being
supplied by the user from the default value."
  (name "" :type string)
  (opts (default-rdb-opts) :type rdb-opts)
  (key-type nil :type (or list symbol))
  (val-type nil :type (or list symbol))
  (sap nil :type (or null alien)))
      
(defmethod close-cf ((self rdb-cf) &optional error)
  (if-let ((sap (rdb-cf-sap self)))
    (setf (rdb-cf-sap self) (rocksdb:rocksdb-column-family-handle-destroy sap))
    (when error (rdb-error "column family is already closed."))))

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
  (cfs (make-array 0 :element-type 'rdb-cf :adjustable t :fill-pointer 0) :type (vector rdb-cf))
  (db nil :type (or null alien))
  (backup nil :type (or null alien))
  (snapshots #() :type (array alien)))

(defvar *default-rdb-opts* (default-rdb-opts))

(defmethod print-object ((self rdb) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":cfs ~A :open ~A" (length (rdb-cfs self)) (db-open-p self))))

(defmethod db ((self rdb))
  (rdb-db self))

(defmethod db-open-p ((self rdb))
  (when (db self) t))

(defmethod db-closed-p ((self rdb))
  (unless (db self) t))

(defun translate-cf-to-field (cf)
  (let ((vt (or (rdb-cf-val-type cf) 'octet-vector))
        (kt (unless (rdb-cf-val-type cf) (or (rdb-cf-key-type cf) 'octet-vector))))
    (make-field :name (rdb-cf-name cf)
                :type (if kt
                          (cons kt vt)
                          vt))))

(defmethod load-field ((self rdb-cf) (field field))
  (let ((type (field-type field)))
  (typecase type
    ;; note that this means you can't use LOAD-SCHEMA to reset an
    ;; rdb schema as you may expect.
    (null nil)
    (atom (setf (rdb-cf-val-type self) type))
    (list (setf (rdb-cf-key-type self) (car type)
                (rdb-cf-val-type self)
                (if (and (listp (cdr type))
                         (= 1 (length (cdr type))))
                    (cadr type)
                    (cdr type)))))
    self))

(defmethod load-schema ((self rdb) (schema schema))
  "Load SCHEMA into rdb database object SELF. This will add any missing rdb-cfs
and update existing key/value types for cfs with the same name. Existing cfs
only get their their type slots updated on non-nil values."
  (loop for field across (fields schema)
        do (if-let ((cf (find-cf (field-name field) self)))
             (load-field cf field)
             (push-cf
              (load-field (make-rdb-cf (field-name field)) field)
              self)))
  self)

(defmethod derive-schema ((self rdb))
  (apply 'make-schema
         (loop for cf across (rdb-cfs self)
               collect (translate-cf-to-field cf))))

(defun create-db (name &key opts cfs schema open)
  "Construct a new RDB instance from NAME.

OPTS = rdb-opts
CFS = (sequence rdb-cf)
SCHEMA = rdb-schema
OPEN = boolean

CFS are always added before the SCHEMA which is loaded with LOAD-SCHEMA.

When OPEN is non-nil, the database and all column families are opened and
internal sap slots are initialized."
  ;; (when (probe-file name) (log:trace! "db exists: " name))
  (let* ((opts (or opts (default-rdb-opts)))
         (obj
           (make-rdb
            (string-right-trim '(#\/)
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
    (when schema
      (load-schema obj schema))
    (when open
      (open-db obj))
    obj))

(defmethod backfill-opts ((self rdb) &key full)
  (with-slots (opts) self
    (if full
        (loop for k across *rocksdb-options*
              unless (opt-no-setter-p k)
              do (pull-sap opts k))
        (pull-sap* opts))
    (rdb-opts-table opts)))

(defmethod push-cf ((cf rdb-cf) (db rdb))
  (vector-push-extend cf (rdb-cfs db)))

(defmethod create-cf ((db rdb) (cf rdb-cf))
  (create-cf-raw (rdb-db db) (rdb-cf-name cf) (rdb-opts-sap (rdb-opts db))))

(defmethod open-cfs ((db rdb) &rest names)
  (let ((cf-names) (cf-opts))
    (loop for cf across (rdb-cfs db)
          do (let ((name (rdb-cf-name cf)))
               (when (or (not names) (member name names :test 'string=))
                   (push name cf-names)
                   (push (rdb-opts-sap (rdb-cf-opts cf)) cf-opts)))
          finally 
          (setf cf-names (nreverse cf-names) 
                cf-opts (nreverse cf-opts)))
    (multiple-value-bind (db-sap cfs) (open-cfs-raw (rdb-opts db) (rdb-name db) cf-names cf-opts)
      (setf (rdb-db db) db-sap)
      (loop for cf across (rdb-cfs db)
            with i = 0
            do (setf (rdb-cf-sap cf) (deref cfs i))
            do (incf i))
      db)))

(defmethod close-cfs ((self rdb))
  (loop for cf across (rdb-cfs self)
        do (close-cf cf)))

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
      (setf backup (close-backup-engine-raw backup)))))

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
  (when cf
    (setf cf (etypecase cf
               (rdb-cf (rdb-cf-sap cf))
               (string (rdb-cf-sap (find-cf cf self)))
               (alien cf))))
  (unless-null-db () self
    (make-instance 'rdb-iter :sap (if cf
                                      (create-cf-iter-raw db cf opts)
                                      (create-iter-raw db opts)))))

(defmethod print-stats ((self rdb) &optional stream)
  (print (rocksdb-options-statistics-get-string (rdb-opts-sap (rdb-opts self))) stream))

(defmethod flush-db ((self rdb) &key) ;; todo flushopts
  (flush-db-raw (rdb-db self)))

(defmethod sync-db ((self rdb) (other null) &key)
  (flush-db self))

(defmethod shutdown-db ((self rdb) &key wait)
  (log:trace! "shutting down database" (rdb-name self))
  (when-let ((db (rdb-db self)))
    (rocksdb-cancel-all-background-work db wait)
    (close-db self)))

(defmethod create-cfs ((self rdb) &key &allow-other-keys)
  (if (null (rdb-db self))
      (warn 'db-missing :message "ignoring attempt to create column-families before opening")
      (loop for cf across (rdb-cfs self)
            do (create-cf self cf))))

(defmethod find-cf ((cf string) (self rdb) &key)
  "Find a CF by name."
  (find cf (rdb-cfs self) :key 'rdb-cf-name :test 'equal))

(defmethod ingest-db ((self rdb) (files list) &key cf (opts (rocksdb-ingestexternalfileoptions-create)))
  (if cf
      (ingest-db-cf-raw (rdb-db self) (find-cf cf self) files opts)
      (ingest-db-raw (rdb-db self) files opts)))

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
      (setf db (close-db-raw db)))))

(defmethod destroy-db ((self rdb))
  ;; close all handles before destruction ensues
  (close-db self)
  (destroy-db-raw (rdb-name self)))

(defmethod put-key ((self rdb) (key t) (val t))
  (put-kv-raw
   (rdb-db self)
   key
   val))

(defmethod put-key ((self rdb) (key string) (val string))
  (put-kv-raw
   (rdb-db self)
   (sb-ext:string-to-octets key)
   (sb-ext:string-to-octets val)))

(defmethod put-kv ((self rdb) (kv kv))
  (put-kv-raw
   (rdb-db self)
   (kv-key kv)
   (kv-val kv)))

(defmethod insert-key ((self rdb) key val &key cf)
  (if-let ((cf (and cf (find-cf cf self))))
    (if-let ((sap (rdb-cf-sap cf)))
      (put-cf-raw
       (rdb-db self)
       sap
       key
       val
       (rocksdb-writeoptions-create))
      (rdb-error "column-family is not open"))
      (put-key self key val)))

(defmethod insert-key ((self rdb) (key string) (val string) &key cf)
  (insert-key self (string-to-octets key) (string-to-octets val) :cf cf))

(defmethod insert-key ((self rdb) (key string) val &key cf)
  (insert-key self (string-to-octets key) val :cf cf))

(defmethod insert-key ((self rdb) key (val string) &key cf)
  (insert-key self key (string-to-octets val) :cf cf))

(defmethod insert-kv ((self rdb) (kv kv) &key cf (opts (rocksdb-writeoptions-create)))
  (if cf
      (let ((cf (etypecase cf
                  (rdb-cf cf)
                  (t (find cf (rdb-cfs self)
                           :key #'rdb-cf-name
                           :test #'equal)))))
        (put-cf-raw (rdb-db self)
                    (rdb-cf-sap cf)
                    (kv-key kv)
                    (kv-val kv)
                    opts))
      (put-kv self kv)))

(defmethod get-key ((self rdb) (key string) &key (opts (rocksdb-readoptions-create)) cf)
  (with-slots (db) self
    (if cf
        (get-cf-str-raw db (rdb-cf-sap (find-cf cf self)) key opts)
        (get-kv-str-raw db key opts))))

(defmethod get-key ((self rdb) key &key (opts (rocksdb-readoptions-create)) cf)
  (with-slots (db) self
    (if cf
        (get-cf-raw db (rdb-cf-sap (find-cf cf self)) key opts)
        (get-kv-raw db key opts))))
