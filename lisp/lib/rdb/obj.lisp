(in-package :rdb)

;;; rdb-opts
(flet ((%mktbl (accessor opts)
         (let ((table (make-hash-table :test #'equal)))
           (mapc (lambda (x) (setf (gethash (car x) table) (cdr x)))
                 (loop for y across opts
                       collect (cons y (format nil "~:@(~A-set-~x~)" accessor y))))
           table)))
  (defvar *rdb-opts-table*
    (%mktbl 'rocksdb-options *rocksdb-options*))
  (defvar *rdb-readopts-table*
    (%mktbl 'rocksdb-readoptions *rocksdb-readoptions*))
  (defvar *rdb-writeopts-table*
    (%mktbl 'rocksdb-writeoptions *rocksdb-writeoptions*))
  (defvar *rdb-backupopts-table*
    (%mktbl 'rocksdb-backup-engine-options *rocksdb-backup-engine-options*))
  (defvar *rdb-compactopts-table*
    (%mktbl 'rocksdb-compactoptions *rocksdb-compactoptions*)))

(macrolet ((%def-opt (name &rest set-only)
             `(progn
                (defun ,(symbolicate '%set- name) (opt key val)
                  (funcall (,(symbolicate name '-setter) key) opt val))
                (defun ,(symbolicate '%get- name) (opt key)
                  (if-let ((g (,(symbolicate name '-getter) key)))
                    (funcall g opt)
                    (warn 'opt-handler-missing :message key)))
                (defun ,(symbolicate '% name '-no-getter-p) (key)
                  (let ((k (typecase key
                             (string (string-downcase key))
                             (symbol (string-downcase (symbol-name key)))
                             (t (string-downcase (format nil "~s" key))))))
                    (memq t (mapcar
                             (lambda (x) (equal k x))
                             ',set-only)))))))
  (%def-opt rdb-opt "parallelism" "enable-statistics")
  (%def-opt rdb-readopt)
  (%def-opt rdb-writeopt)
  (%def-opt rdb-backupopt)
  (%def-opt rdb-compactopt))

(macrolet ((define-rdb-opt-struct (name opts creator &rest defaults)
             (let ((%name (symbolicate (string-right-trim "S" name)))
                   (%make (symbolicate '%make- name)))
               `(prog1
                  (defstruct (,name (:constructor ,%make))
                    (table (make-hash-table :test 'equal) :type hash-table)
                    (sap nil :type (or null alien)))
                  (eval-always
                    (defun ,(symbolicate 'make- name) (&rest opts)
                      (let ((obj (,%make :sap (,creator))))
                        (loop for (k v) on opts by #'cddr while v
                              do (let ((k (typecase k
                                            (string (string-downcase k))
                                            (symbol (string-downcase (symbol-name k)))
                                            (t (string-downcase (format nil "~s" k))))))
                                   (setf (db-opt obj k) v)))
                        (push-sap* obj)
                        obj)))                  
                  (defun ,(symbolicate 'make- name '*) (alien)
                    ,(format nil "Coerce ALIEN into a ~A struct. This function doesn't populate the
values in Lisp, just binds the sap." name)
                    (,%make :sap alien))
                  (defaccessor* db-opt 
                      ((self ,name) key)
                    (gethash key (db-opts self))
                    (val (self ,name) key &key push)
                    (prog1 (setf (gethash key (db-opts self)) val)
                      (when push (push-sap self key))))
                  (defmethod push-sap ((self ,name) key)
                    "Push KEY from slot :TABLE to the instance :SAP."
                    (,(symbolicate '%set- %name) (sap self) key (db-opt self key)))
                  (defmethod push-sap* ((self ,name))
                    "Initialized the SAP slot with values from TABLE."
                    (loop for k in (hash-table-keys (db-opts self))
                          ;; note how we don't handle any special cases here - we can
                          ;; always set an opt but sometimes we can't get it.
                          do (push-sap self k)))
                  (defmethod pull-sap ((self ,name) key)
                    (setf (gethash key (db-opts self)) (,(symbolicate '%get- %name) (sap self) key)))
                  (defmethod pull-sap* ((self ,name))
                    (let ((table (db-opts self)))
                      (loop for k in (hash-table-keys table)
                            unless (,(symbolicate '% %name '-no-getter-p) k)
                            do (pull-sap self k))
                      table))
                  (defmethod backfill-opts ((self ,name) &key full)
                    "Backfill the TABLE slot with values from SAP.

When FULL is non-nil, retrieve the full set of options available, not
just the keys currently present in TABLE."
                    (if full
                        (loop for k across ,opts
                              unless (,(symbolicate '% %name '-no-getter-p) k)
                              do (pull-sap self k))
                        (pull-sap* self))
                    (db-opts self))
                  ;; (defun ,(symbolicate 'default- name) ())
                  (defaccessor (sap) ((self ,name)) (,(symbolicate name '-sap) self))
                  (defaccessor (db-opts) ((self ,name)) (,(symbolicate name '-table) self))
                  (defun ,(symbolicate 'default- name) ()
                    (,(symbolicate 'make- name) ,@defaults))
                  (defvar ,(symbolicate '*default- name '*) (,(symbolicate 'default- name)))))))
  (define-rdb-opt-struct rdb-opts *rocksdb-options* rocksdb-options-create
    :create-if-missing t :create-missing-column-families t :parallelism (num-cpus))
  (define-rdb-opt-struct rdb-readopts *rocksdb-readoptions* rocksdb-readoptions-create)
  (define-rdb-opt-struct rdb-writeopts *rocksdb-writeoptions* rocksdb-writeoptions-create)
  (define-rdb-opt-struct rdb-compactopts *rocksdb-compactoptions* rocksdb-compactoptions-create)
  (define-rdb-opt-struct rdb-backupopts *rocksdb-backup-engine-options* rocksdb-backup-engine-options-create))

(defmethod set-db-opt ((self t) key val &key push)
  (setf (db-opt self key :push push) val))

(defvar *default-kv* (make-kv))

;;; Iterator
(defstruct rdb-iter (sap nil :type (or null alien)))
(defaccessor (sap) ((self rdb-iter)) (rdb-iter-sap self))

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
(defstruct (rdb-cf (:constructor make-rdb-cf (name &key opts sap)))
  "RDB Column Family structure. Contains a name, db-opts,
and a system-area-pointer to the underlying rocksdb_cf_t handle."
  (name "" :type string)
  (opts (default-rdb-opts) :type rdb-opts)
  (sap nil :type (or null alien)))
      
(defaccessor (column-opts) ((self rdb-cf)) (rdb-cf-opts self))
(defaccessor (sap) ((self rdb-cf)) (rdb-cf-sap self))
(defaccessor (name) ((self rdb-cf)) (rdb-cf-name self))

(defmethod close-column ((self rdb-cf) &optional error)
  (if-let ((sap (sap self)))
    (setf (sap self) (rocksdb:rocksdb-column-family-handle-destroy sap))
    (when error (rdb-error "column family is already closed."))))

(defmethod merge-key ((self rdb-cf) key val &key db (opts (rocksdb-writeoptions-create)))
  (merge-cf-raw (sap db) (sap self) key val opts))

(defmethod merge-kv ((self rdb-cf) kv &key db (opts (rocksdb-writeoptions-create)))
  (merge-cf-raw (sap db) (sap self) (kv-key kv) (kv-val kv) opts))

;;; rdb-stats
(defstruct (rdb-stats (:constructor make-rdb-stats (&optional sap)))
  (sap nil :type (or null alien)))
(defaccessor (sap) ((self rdb-stats)) (rdb-stats-sap self))

;;; metadata
(defstruct rdb-cf-metadata
  (name "default" :type string)
  (size 0 :type fixnum)
  (level-count 7 :type fixnum)
  (file-count 0 :type fixnum)
  (sap nil :type (or null alien)))
(defaccessor (sap) ((self rdb-cf-metadata)) (rdb-cf-metadata-sap self))

(defmethod db-metadata ((self rdb-cf-metadata) &optional (level 0))
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
(defaccessor (sap) ((self rdb-level-metadata)) (rdb-level-metadata-sap self))
  
(defmethod db-metadata ((self rdb-level-metadata) &optional (file 0))
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
(defaccessor (sap) ((self rdb-sst-file-metadata)) (rdb-sst-file-metadata-sap self))
  
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

;;; SST
(defstruct (sst-file-writer (:constructor %make-sst-file-writer (sap)))
  (sap nil :type (or null alien)))

(defun make-sst-file-writer (&optional comparator
                               (env-opts (rocksdb-envoptions-create))
                               (io-opts (rocksdb-options-create)))
  (%make-sst-file-writer
   (if comparator
       (create-sst-writer-with-comparator-raw comparator env-opts io-opts)
       (create-sst-writer-raw env-opts io-opts))))

(defun sst-file-size (writer)
  (declare (sst-file-writer writer))
  (sst-file-size-raw (sst-file-writer-sap writer)))

(defun open-sst (writer path)
  (declare (sst-file-writer writer))
  (open-sst-writer-raw (sst-file-writer-sap writer) path))

(defun finish-sst (writer)
  (declare (sst-file-writer writer))
  (finish-sst-writer-raw (sst-file-writer-sap writer)))

(defun destroy-sst (writer)
  (declare (sst-file-writer writer))
  (with-slots (sap) writer
    (unless (null sap)
      (destroy-sst-writer-raw sap)
      (setf sap nil))))

(defmethod print-object ((self sst-file-writer) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":size ~A" (when (sst-file-writer-sap self) (sst-file-size self)))))

(defmethod put-key ((self sst-file-writer) key val)
  (sst-put-raw (sst-file-writer-sap self) key val))

(defmethod put-key ((self sst-file-writer) (key simple-string) (val simple-string))
  (sst-put-str-raw (sst-file-writer-sap self) key val))

(defmethod put-kv ((self sst-file-writer) (kv kv))
  (sst-put-raw (sst-file-writer-sap self)
               (kv-key kv) (kv-val kv)))

(defmethod delete-key ((self sst-file-writer) key &key)
  (sst-delete-raw (sst-file-writer-sap self) key))

(defmethod delete-key-ts ((self sst-file-writer) key ts)
  (sst-delete-ts-raw (sst-file-writer-sap self) key ts))

(defmethod delete-key-range ((self sst-file-writer) start end &key)
  (sst-delete-range-raw (sst-file-writer-sap self) start end))

(defmethod put-key-ts ((self sst-file-writer) key val ts)
  (sst-put-ts-raw (sst-file-writer-sap self) key val ts))

;;; rdb
(defstruct rdb
  (name "" :type string)
  (opts (default-rdb-opts) :type rdb-opts)
  (cfs (make-array 0 :element-type 'rdb-cf :adjustable t) :type (vector rdb-cf))
  (sap nil :type (or null alien))
  (snapshots #() :type (array alien)))

(defaccessor (sap) ((self rdb)) (rdb-sap self))
(defaccessor (name) ((self rdb)) (rdb-name self))
(defaccessor (columns) ((self rdb)) (rdb-cfs self))
(defaccessor (db) ((self rdb)) (sap self))
(defaccessor (db-opts) ((self rdb)) (rdb-opts self))

(defmethod print-object ((self rdb) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":cfs ~A :open ~A" (length (rdb-cfs self)) (db-open-p self))))

(defmethod db-open-p ((self rdb))
  (when (sap self) t))

(defmethod db-closed-p ((self rdb))
  (unless (sap self) t))

(defun translate-cf-to-field (cf)
  (make-field :name (rdb-cf-name cf)
              :type (cons 'octet-vector 'octet-vector)))

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
  (when (probe-file name) (log:trace! "attempting to create existing db: ~A" name))
  (let* ((opts (or opts (default-rdb-opts)))
         (obj
           (make-rdb
            :name 
            (string-right-trim '(#\/)
                               (typecase name
                                 (pathname (namestring name))
                                 (string name)
                                 (t (error "invalid NAME: ~S" name))))
            :opts opts
            :cfs (or (when cfs
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
              unless (%rdb-opt-no-getter-p k)
              do (pull-sap opts k))
        (pull-sap* opts))
    (db-opts opts)))

(defmethod add-column ((cf rdb-cf) (db rdb))
  (vector-push-extend cf (rdb-cfs db)))

(defmethod create-column ((db rdb) (cf rdb-cf))
  (create-cf-raw (rdb-sap db) (rdb-cf-name cf) (sap (rdb-opts db))))

(defmethod open-columns ((db rdb) &rest names)
  (let ((cf-names) (cf-opts))
    (loop for cf across (rdb-cfs db)
          do (let ((name (rdb-cf-name cf)))
               (when (or (not names) (member name names :test 'string=))
                   (push name cf-names)
                   (push (sap (column-opts cf)) cf-opts)))
          finally 
          (setf cf-names (nreverse cf-names) 
                cf-opts (nreverse cf-opts)))
    (multiple-value-bind (db-sap cfs) (open-cfs-raw (rdb-opts db) (rdb-name db) cf-names cf-opts)
      (setf (rdb-sap db) db-sap)
      (loop for cf across (rdb-cfs db)
            with i = 0
            do (setf (rdb-cf-sap cf) (deref cfs i))
            do (incf i))
      db)))

(defmethod close-columns ((self rdb))
  (loop for cf across (rdb-cfs self)
        do (close-column cf)))

(defmacro unless-null-db (slots self &body body)
  `(with-slots (sap ,@slots) ,self
     (unless (null sap)
       ,@body)))

(defmethod destroy-column ((cf rdb-cf))
  (with-slots (sap) cf
    (unless (null sap)
      (setf sap (destroy-cf-raw sap)))))

(defaccessor* db-opt 
    ((self rdb) key) (db-opt (db-opts self) key)
    (new (self rdb) key &key push)
  (prog1 (setf (db-opt (db-opts self) key) new)
    (when push (push-sap (db-opts self) key))))
  
(defmethod push-opts ((self rdb))
  (with-slots (opts) self
    (push-sap* opts)))

(defmethod open-db ((self rdb))
  (with-slots (name sap opts) self
    (if sap
        (progn
          (cerror "Ignore and continue" 'open-db-error 
                  :db sap
                  :message "Database is already open")
          sap)
        (setf sap (open-db-raw name (sap opts))))))

(defmethod db-prop ((self rdb) (propname string))
  (unless-null-db () self
    (rocksdb-property-value sap propname)))

(defmethod repair-db ((self rdb) &key)
  (repair-db-raw (rdb-name self)))

(defmethod open-backup-db ((self rdb) &key path)
  (with-slots (opts) self
    (open-backup-engine-raw path (sap opts))))

(defmethod backup-db ((self rdb) &key path)
  (unless-null-db (opts) self
    (if (null path)
        (error 'open-backup-engine-error :db sap 
                                         :message "PATH must not be nil when no backups exist")
        (create-new-backup-raw (open-backup-db self :path path) sap))))

(defmethod restore-db ((self rdb) (from string) &key id opts)
  (unless-null-db (name) self
    (restore-from-backup-raw (open-backup-db self :path from) name from id opts)))

(defmethod snapshot-db ((self rdb))
  (unless-null-db (snapshots) self
    (vector-push-extend (create-snapshot-raw sap) snapshots)))

(defmethod db-metadata ((self rdb) &optional cf)
  (make-rdb-cf-metadata :sap (get-metadata-raw (rdb-sap self) cf)))

(defmethod db-stats ((self rdb) &optional (htype (rocksdb-statistics-level "all")))
  (make-rdb-stats (get-stats-raw (sap (rdb-opts self)) htype)))

(defmethod create-iter ((self rdb) &optional cf (opts (rocksdb-readoptions-create)))
  (when cf
    (setf cf (etypecase cf
               (rdb-cf (rdb-cf-sap cf))
               (string (rdb-cf-sap (find-column cf self)))
               (alien cf))))
  (unless-null-db () self
    (make-rdb-iter :sap (if cf
                            (create-cf-iter-raw sap cf opts)
                            (create-iter-raw sap opts)))))

(defmethod print-stats ((self rdb) &optional stream)
  (print (rocksdb-options-statistics-get-string (sap (rdb-opts self))) stream))

(defmethod flush-db ((self rdb) &key wait)
  (flush-db-raw (rdb-sap self) wait))

(defmethod sync-db ((self rdb) (other null) &key wait)
  (flush-db self :wait wait))

(defmethod shutdown-db ((self rdb) &key wait)
  (log:trace! "shutting down database" (rdb-name self))
  (when-let ((db (rdb-sap self)))
    (rocksdb-cancel-all-background-work db wait)
    (close-db self)))

(defmethod create-columns ((self rdb))
  (if (null (rdb-sap self))
      (warn 'db-missing :message "ignoring attempt to create column-families before opening")
      (loop for cf across (rdb-cfs self)
            do (create-column self cf))))

(defmethod find-column ((cf string) (self rdb) &key)
  "Find a CF by name."
  (find cf (rdb-cfs self) :key 'name :test 'equal))

(defmethod ingest-db ((self rdb) (files list) &key cf (opts (rocksdb-ingestexternalfileoptions-create)))
  (if cf
      (ingest-db-cf-raw (rdb-sap self) (find-column cf self) files opts)
      (ingest-db-raw (rdb-sap self) files opts)))

(defmethod destroy-columns ((self rdb) &key &allow-other-keys)
  (with-slots (cfs) self
    (declare (type (array rdb-cf) cfs))
    (loop for cf across cfs
          do (setf cf (destroy-column cf)))))

(defmethod close-db ((self rdb) &key &allow-other-keys)
  (with-slots (sap cfs) self
    ;; (close-backup-db self)
    ;; (unless (zerop (length snapshots))
    ;;   (loop for s across snapshots do (release-snapshot-raw sap s)))
    (destroy-columns self)
    (unless (null sap)
      (close-db-raw sap)
      (setf (sap self) nil))))

(defmethod destroy-db ((self rdb))
  ;; close all handles before destruction ensues
  (close-db self)
  (destroy-db-raw (rdb-name self)))

(defmethods put-key 
  (((self rdb) (key t) (val t))
   (put-kv-raw
    (rdb-sap self)
    key
    val))
  (((self rdb) (key string) (val string))
   (put-kv-raw
    (rdb-sap self)
    (sb-ext:string-to-octets key)
    (sb-ext:string-to-octets val))))

(defmethod put-kv ((self rdb) (kv kv))
  (put-kv-raw
   (sap self)
   (kv-key kv)
   (kv-val kv)))

(defmethods insert-key 
  (((self rdb) key val &key cf)
   (if-let ((cf (and cf (find-column cf self))))
     (if-let ((sap (rdb-cf-sap cf)))
       (put-cf-raw
        (rdb-sap self)
        sap
        key
        val
        (rocksdb-writeoptions-create))
       (rdb-error "column-family is not open"))
     (put-key self key val)))
  (((self rdb) (key string) (val string) &key cf)
   (insert-key self (string-to-octets key) (string-to-octets val) :cf cf))
  (((self rdb) (key string) val &key cf)
   (insert-key self (string-to-octets key) val :cf cf))
  (((self rdb) key (val string) &key cf)
   (insert-key self key (string-to-octets val) :cf cf)))

(defmethod insert-kv ((self rdb) (kv kv) &key cf (opts (rocksdb-writeoptions-create)))
  (if cf
      (let ((cf (etypecase cf
                  (rdb-cf cf)
                  (t (find cf (rdb-cfs self)
                           :key #'rdb-cf-name
                           :test #'equal)))))
        (put-cf-raw (rdb-sap self)
                    (rdb-cf-sap cf)
                    (kv-key kv)
                    (kv-val kv)
                    opts))
      (put-kv self kv)))

(defmethods get-val 
  (((self rdb) (key string) &key (opts (rocksdb-readoptions-create)) cf)
   (with-slots (sap) self
     (if cf
         (get-cf-str-raw sap (rdb-cf-sap (find-column cf self)) key opts)
         (get-kv-str-raw sap key opts))))
  (((self rdb) key &key (opts (rocksdb-readoptions-create)) cf)
   (with-slots (sap) self
     (if cf
         (get-cf-raw sap (rdb-cf-sap (find-column cf self)) key opts)
         (get-kv-raw sap key opts)))))

(defmethod get-value ((self rdb) key)
  (get-kv-raw (sap self) key (rocksdb-readoptions-create)))

(defmethod merge-key ((self rdb) key val &key (opts (rocksdb-writeoptions-create)))
  (merge-kv-raw (sap self) key val opts))

(defmethod merge-kv ((self rdb) kv &key (opts (rocksdb-writeoptions-create)))
  (merge-kv-raw (sap self) (kv-key kv) (kv-val kv) opts))

;;; Transaction DB
(defstruct rdb-transaction-db 
  sap 
  snapshots 
  (opts (rocksdb-transactiondb-options-create)))

(defaccessor (sap) ((self rdb-transaction-db)) (rdb-transaction-db-sap self))
(defaccessor (db-opts) ((self rdb-transaction-db)) (rdb-transaction-db-opts self))

(defmethod open-transaction-db ((self rdb) &key path (opts (rocksdb-transactiondb-options-create)))
  (make-rdb-transaction-db
   :sap (open-transactiondb-raw (sap (db-opts self)) opts path)
   :opts opts))

(defmethod close-transaction-db ((self rdb-transaction-db))
  (rocksdb-transactiondb-close (sap self)))

(defmethods get-val
  (((self rdb-transaction-db) (key string) &key (opts (rocksdb-readoptions-create)) cf pinned)
   (let ((sap (sap self)))
     (if cf
         (transactiondb-get-cf-str-raw sap (rdb-cf-sap (find-column cf self)) key opts pinned)
         (transactiondb-get-kv-str-raw sap key opts pinned))))
  (((self rdb) key &key (opts (rocksdb-readoptions-create)) cf pinned)
   (with-slots (sap) self
     (if cf
         (get-cf-raw sap (rdb-cf-sap (find-column cf self)) key opts pinned)
         (get-kv-raw sap key opts pinned)))))

(defmethod get-value ((self rdb-transaction-db) key)
  (transactiondb-get-kv-raw self key))

;;; Transaction
(defstruct rdb-transaction sap savepoint)
(defaccessor (sap) ((self rdb-transaction)) (rdb-transaction-sap self))
(defaccessor (name) ((self rdb-transaction)) (transaction-name-raw (sap self)))

(defmethod transaction-object-p ((self rdb-transaction)) t)

(defmethod make-transaction ((self rdb-transaction-db)
                             &key name
                                  txn
                                  (opts (rocksdb-transaction-options-create))
                                  (wopts (rocksdb-writeoptions-create)))
  (let ((obj (make-rdb-transaction
              :sap (rocksdb-transaction-begin (sap self) wopts opts txn))))
    (when name (setf (name obj) name))
    obj))

(defmethod prepare-transaction ((self rdb-transaction) &key)
  (prepare-transaction-raw (sap self)))

(defmethod rollback-transaction ((self rdb-transaction) &key savepoint)
  (rollback-transaction-raw (sap self) savepoint))

(defmethod abort-transaction ((self rdb-transaction) &key)
  (rollback-transaction self)
  (rocksdb-transaction-destroy (sap self)))

(defmethod commit-transaction ((self rdb-transaction) &key)
  (commit-transaction-raw (sap self)))

;;; Secondary DB
(defstruct rdb-secondary-db sap opts)
(defaccessor (sap) ((self rdb-secondary-db)) (rdb-secondary-db-sap self))
(defaccessor (db-opts) ((self rdb-secondary-db)) (rdb-secondary-db-opts self))

(defmethod open-secondary-db ((self rdb) &key path opts)
  (make-rdb-secondary-db 
   :sap (open-db-secondary-raw opts (name self) path)
   :opts opts))

(defmethod close-secondary-db ((self rdb-secondary-db))
  (rocksdb-close (sap self)))

;;; Backup DB
(defstruct rdb-backup-db sap opts)
(defaccessor (sap) ((self rdb-backup-db)) (rdb-backup-db-sap self))
(defaccessor (db-opts) ((self rdb-backup-db)) (rdb-backup-db-opts self))

(defmethod open-backup-db ((self rdb-backup-db) &key path)
  (setf (sap self) (open-backup-engine-raw path (db-opts self))))
        
(defmethod close-backup-db ((self rdb-backup-db))
  (close-backup-engine-raw (sap self)))

;;; Write Batches
(defstruct rdb-writebatch sap)
(defaccessor (sap) ((self rdb-writebatch)) (rdb-writebatch-sap self))

(defstruct rdb-wbwi sap wb reserved overwrite-key data savepoints params)
(defaccessor (sap) ((self rdb-wbwi)) (rdb-wbwi-sap self))

;;; Env
(defstruct rdb-env sap path threads)
(defaccessor (sap) ((self rdb-env)) (rdb-env-sap self))
(defaccessor (path) ((self rdb-env)) (rdb-env-path self))

;;; Merge Ops
(defun concat-merge-op ()
  (rocksdb-mergeoperator-create 
   nil
   (alien-sap (alien-callable-function 'rocksdb-destructor))
   (alien-sap (alien-callable-function 'rocksdb-concat-full-merge))
   (alien-sap (alien-callable-function 'rocksdb-concat-partial-merge))
   (alien-sap (alien-callable-function 'rocksdb-delete-value))
   (alien-sap (alien-callable-function 'rocksdb-concat-merge-name))))

(defun index-merge-op ()
  (rocksdb-mergeoperator-create 
   nil
   (alien-sap (alien-callable-function 'rocksdb-destructor))
   (alien-sap (alien-callable-function 'rocksdb-index-full-merge))
   (alien-sap (alien-callable-function 'rocksdb-index-partial-merge))
   (alien-sap (alien-callable-function 'rocksdb-delete-value))
   (alien-sap (alien-callable-function 'rocksdb-index-merge-name))))

;;; Logger
(defun rdb-log-default (level &optional prefix)
  (if prefix
      (rocksdb-logger-create-stderr-logger level prefix)
      (rocksdb-logger-create-callback-logger 
       level 
       (alien-sap (alien-callable-function 'rocksdb-log-default)) 
       nil)))
