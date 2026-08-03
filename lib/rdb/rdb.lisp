;;; rdb.lisp --- RocksDB Low-level Structures

;; 

;;; Code:
(in-package :rdb)

;;; rdb-opts
;; TODO 2026-08-02: this whole rdb-opts thing needs work - eliminate struct
;; wrappers and use our 'options' api.
(flet ((%mktbl (accessor opts)
         (let ((table (make-hash-table :test #'equal)))
           (mapc (lambda (x) (setf (gethash (car x) table) (cdr x)))
                 (loop for y across opts
                       collect (cons y (format nil "~:@(~A-set-~x~)" accessor y))))
           table)))
  (defvar *rdb-opts-table*
    (let ((tbl (%mktbl 'rocksdb-options *rocksdb-options*)))
      (setf (gethash "event-listener" tbl) "ROCKSDB-OPTIONS-ADD-EVENTLISTENER")
      tbl))
  (defvar *rdb-readopts-table*
    (%mktbl 'rocksdb-readoptions *rocksdb-readoptions*))
  (defvar *rdb-writeopts-table*
    (%mktbl 'rocksdb-writeoptions *rocksdb-writeoptions*))
  (defvar *rdb-backupopts-table*
    (%mktbl 'rocksdb-backup-engine-options *rocksdb-backup-engine-options*))
  (defvar *rdb-ingestopts-table*
    (%mktbl 'rocksdb-ingestexternalfileoptions *rocksdb-ingestexternalfileoptions*))
  (defvar *rdb-compactopts-table*
    (%mktbl 'rocksdb-compactoptions *rocksdb-compactoptions*)))

(eval-always
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
    (%def-opt rdb-opt "parallelism" "enable-statistics" "event-listener")
    (%def-opt rdb-readopt)
    (%def-opt rdb-writeopt)
    (%def-opt rdb-backupopt)
    (%def-opt rdb-compactopt)
    (%def-opt rdb-ingestopt))

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
                          obj))
                      (defun ,(symbolicate 'make- name '*) (alien)
                        ,(format nil "Coerce ALIEN into a ~A struct. This function doesn't populate the
values in Lisp, just binds the sap." name)
                        (,%make :sap alien))
                      ;; db-opt accessors
                      (defaccessor* db-opt
                          ((self ,name) key)
                          (gethash key (db-opts self))
                          (val (self ,name) key &key push)
                        (prog1 (setf (gethash key (db-opts self)) val)
                          (when push (push-sap self key))))
                      (defaccessor db-opts ((self ,name)) (,(symbolicate name '-table) self))
                      ;; ast accessors
                      (defmethod ast ((self ,name))
                        (let ((lst))
                          (maphash 
                           (lambda (k v) (nconsc lst (list (keywordicate (string-upcase k)) v)))
                           (db-opts self))
                          lst))
                      (defmethod (setf ast) (new (self ,name))
                        (setf (db-opts self) 
                              (let ((tbl (make-hash-table :test 'equal)))
                                (doplist (k v) new
                                  (setf (gethash k tbl) v))
                                tbl)))
                      ;; sap accessors
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
                      (defaccessor sap ((self ,name)) (,(symbolicate name '-sap) self))
                      ;; default function and special var
                      (defun ,(symbolicate 'default- name) ()
                        (,(symbolicate 'make- name) ,@defaults))
                      (defvar ,(symbolicate '*default- name '*) (,(symbolicate 'default- name))))))))
    (define-rdb-opt-struct rdb-opts *rocksdb-options* rocksdb-options-create
      :create-if-missing t 
      :create-missing-column-families t 
      :parallelism (num-cpus)
      :compression (rocksdb-compression-type :zstd))
    (define-rdb-opt-struct rdb-readopts *rocksdb-readoptions* rocksdb-readoptions-create)
    (define-rdb-opt-struct rdb-writeopts *rocksdb-writeoptions* rocksdb-writeoptions-create)
    (define-rdb-opt-struct rdb-compactopts *rocksdb-compactoptions* rocksdb-compactoptions-create)
    (define-rdb-opt-struct rdb-backupopts *rocksdb-backup-engine-options* rocksdb-backup-engine-options-create)))

(defvar *default-kv* (make-kv))

;;; Iterator
(defstruct rdb-iter 
  (sap nil :type (or null (alien (* rocksdb-iterator)))))

(defaccessor sap ((self rdb-iter)) (rdb-iter-sap self))

(defmethod free ((self rdb-iter))
  (rocksdb-iter-destroy (sap self)))

(defmethod reset ((self rdb-iter) &key)
  (with-errptr e
    (rocksdb-iter-refresh (sap self) e)))

(defmethod iter-valid-p ((self rdb-iter))
  (rocksdb-iter-valid (sap self)))

(defmethod seek-to-first ((self rdb-iter))
  (rocksdb-iter-seek-to-first (rdb-iter-sap self)))

(defmethod seek-to-last ((self rdb-iter))
  (rocksdb-iter-seek-to-last (rdb-iter-sap self)))

(defmethod seek-for-prev ((self rdb-iter) (key vector) &key)
  (rocksdb-iter-seek-for-prev (rdb-iter-sap self) (octets-to-alien key) (length key)))

(defmethod seek ((self rdb-iter) (key simple-vector) &key)
  (rocksdb-iter-seek (rdb-iter-sap self) (octets-to-alien key) (length key)))

(defmethod next ((self rdb-iter))
  (rocksdb-iter-next (rdb-iter-sap self)))

(defmethod prev ((self rdb-iter))
  (rocksdb-iter-prev (rdb-iter-sap self)))

(defmethod skey ((self rdb-iter))
  (with-alien ((klen size-t))
    (let ((key (rocksdb-iter-key (rdb-iter-sap self) (addr klen))))
      (let ((k (make-octets klen)))
        (clone-octets-from-alien key k)
        (values
         k
         klen)))))

(defmethod sval ((self rdb-iter))
  (with-alien ((vlen size-t))     
    (let ((val (rocksdb-iter-value (sap self) (addr vlen))))
      (let ((v (make-octets vlen)))
        (clone-octets-from-alien val v)
        (values
         v
         vlen)))))

(defmethod val ((self rdb-iter)) (sval self))

(defmethod kv ((self rdb-iter))
  (make-kv (skey self) (sval self)))

(defmethod timestamp ((self rdb-iter))
  (with-alien ((tslen size-t))
    (values
     (rocksdb-iter-timestamp (sap self) (addr tslen))
     tslen)))

;;; column family
(defstruct (rdb-cf (:constructor make-rdb-cf (name &key opts sap)))
  "RDB Column Family structure. Contains a name, db-opts,
and a system-area-pointer to the underlying rocksdb_cf_t handle."
  (name "" :type string)
  (opts (default-rdb-opts) :type rdb-opts)
  (sap nil :type (or null (alien (* rocksdb-column-family-handle)))))

(defaccessor column-opts ((self rdb-cf)) (rdb-cf-opts self))
(defaccessor sap ((self rdb-cf)) (rdb-cf-sap self))
(defaccessor name ((self rdb-cf)) (rdb-cf-name self))

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
  (sap nil :type (or null (alien (* rocksdb-statistics-histogram-data)))))

(defaccessor sap ((self rdb-stats)) (rdb-stats-sap self))

;;; metadata
(defstruct rdb-cf-metadata
  (name "default" :type string)
  (size 0 :type fixnum)
  (level-count 7 :type fixnum)
  (file-count 0 :type fixnum)
  (sap nil :type (or null (alien (* rocksdb-column-family-metadata)))))

(defaccessor sap ((self rdb-cf-metadata)) (rdb-cf-metadata-sap self))
(defaccessor name ((self rdb-cf-metadata)) (rdb-cf-metadata-name self))

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
  (sap nil :type (or null (alien (* rocksdb-level-metadata)))))

(defaccessor sap ((self rdb-level-metadata)) (rdb-level-metadata-sap self))

(defmethod db-metadata ((self rdb-level-metadata) &optional (file 0))
  (if (null (sap self))
      (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
      (make-rdb-sst-file-metadata :sap (rocksdb-level-metadata-get-sst-file-metadata (sap self) file))))

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
  (sap nil :type (or null (alien (* rocksdb-sst-file-metadata)))))

(defaccessor sap ((self rdb-sst-file-metadata)) (rdb-sst-file-metadata-sap self))

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

;;; Snapshots
(defstruct rdb-snapshot 
  (sap nil :type (or null (alien (* rocksdb-snapshot)))))

(defaccessor sap ((self rdb-snapshot)) (rdb-snapshot-sap self))
(defmethod id ((self rdb-snapshot)) (rocksdb-snapshot-get-sequence-number (sap self)))

;;; Checkpoints
(defstruct rdb-checkpoint 
  (sap nil :type (or null (alien (* rocksdb-checkpoint))))
  path)

(defaccessor sap ((self rdb-checkpoint)) (rdb-checkpoint-sap self))
(defaccessor path ((self rdb-checkpoint)) (rdb-checkpoint-path self))

(defun %make-checkpoint (rdb &optional path)
  (let ((chk (with-errptr e
               (make-rdb-checkpoint :sap (rocksdb-checkpoint-object-create (sap rdb) e)))))
    (when path (setf (path chk) path))
    chk))

;;; SST
(defstruct (sst-file-writer (:constructor %make-sst-file-writer (sap)))
  (sap nil :type (or null (alien (* rocksdb-sstfilewriter)))))

(defun make-sst-file-writer (&optional comparator
                                       env-opts
                                       io-opts)
  (let ((env (or env-opts (rocksdb-envoptions-create)))
        (io (or io-opts (rocksdb-options-create))))
  (%make-sst-file-writer
   (if comparator
       (create-sst-writer-with-comparator-raw comparator env io)
       (create-sst-writer-raw env io)))))

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
  (sap nil :type (or null (alien (* rocksdb)))))

(defaccessor sap ((self rdb)) (rdb-sap self))
(defaccessor name ((self rdb)) (rdb-name self))
(defaccessor db ((self rdb)) (sap self))
(defaccessor db-opts ((self rdb)) (rdb-opts self))

(defmethod print-object ((self rdb) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":open ~A" (db-open-p self))))

(defmethod db-open-p ((self rdb))
  (when (sap self) t))

(defmethod db-closed-p ((self rdb))
  (unless (sap self) t))

(defun create-rdb (name &key opts schema open)
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
            :opts opts)))
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

(defmethod open-column ((self rdb) (col rdb-cf) &key)
  (ifret (sap col)
         (setf (sap col) (create-column self col))))

(defmethod create-column ((db rdb) (cf rdb-cf))
  (create-cf-raw (sap db) (name cf) (sap (column-opts cf))))

(defmacro unless-null-db (slots self &body body)
  `(with-slots (sap ,@slots) ,self
     (unless (null sap)
       ,@body)))

(defmethod destroy-column ((cf rdb-cf) &optional error)
  (with-slots (sap) cf
    (unless (and (null sap) (when error (std-error "column is already closed")))
      (setf sap (destroy-cf-raw sap)))))

(defaccessor* db-opt
    ((self rdb) key) (db-opt (db-opts self) (string-downcase key))
    (new (self rdb) key &key push)
  (prog1 (setf (db-opt (db-opts self) (string-downcase key)) new)
    (when push (push-sap (db-opts self) (string-downcase key)))))

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
        (setf sap (open-db-raw name (or (sap opts) (push-opts self)))))))

(defmethod db-prop ((self rdb) (propname string))
  (unless-null-db () self
    (rocksdb-property-value sap propname)))

(defmethod repair-db ((self rdb) &key)
  (repair-db-raw (rdb-name self)))

(defmethod open-backup-engine ((self rdb) &key path)
  (with-slots (opts) self
    (open-backup-engine-raw path (sap opts))))

(defmethod backup-db ((self rdb) &key path)
  (unless-null-db (opts) self
    (if (null path)
        (error 'open-backup-engine-error :db sap 
                                         :message "PATH must not be nil when no backups exist")
        (create-new-backup-raw (open-backup-engine self :path path) sap))))

(defmethod restore-db ((self rdb) (from string) &key id opts)
  (unless-null-db (name) self
    (restore-from-backup-raw (open-backup-engine self :path from) name from id opts)))

(defmethod snapshot-db ((self rdb))
  (unless-null-db () self
    (make-rdb-snapshot :sap (create-snapshot-raw sap))))

(defmethod db-metadata ((self rdb) &optional cf)
  (make-rdb-cf-metadata :sap (get-metadata-raw (rdb-sap self) cf)))

(defmethod db-stats ((self rdb) &optional (htype (rocksdb-statistics-level "all")))
  (make-rdb-stats (get-stats-raw (sap (rdb-opts self)) htype)))

(defmethod iter ((self rdb) &key cf (opts (rocksdb-readoptions-create)))
  (let ((col (etypecase cf
               (rdb-cf (rdb-cf-sap cf))
               ;; (string (rdb-cf-sap (find-column cf self)))
               (null nil)
               (alien cf))))
    (unless-null-db () self
      (make-rdb-iter 
       :sap (if col
                (create-cf-iter-raw sap col opts)
                (create-iter-raw sap opts))))))

(defmethod print-stats ((self rdb) &optional stream)
  (if stream
      (println (rocksdb-options-statistics-get-string (sap (rdb-opts self))) stream)
      (with-output-to-string (s)
        (print-stats self s))))

(defmethod flush-db ((self rdb) &key wait)
  (flush-db-raw (rdb-sap self) wait))

(defmethod sync-db ((self rdb) (other null) &key wait)
  (flush-db self :wait wait))

(defmethod shutdown-db ((self rdb) &key wait)
  (log:trace! "shutting down database" (rdb-name self))
  (when-let ((db (rdb-sap self)))
    (rocksdb-cancel-all-background-work db wait)
    (close-db self)))

(defmethod ingest-db ((self rdb) (files list) &key column (opts (rocksdb-ingestexternalfileoptions-create)))
  (if column
      (ingest-db-cf-raw (sap self) (sap column) files opts)
      (ingest-db-raw (sap self) files opts)))

(defmethod close-db ((self rdb) &key &allow-other-keys)
  (with-slots (sap opts) self
    (unless (null sap)
      (close-db-raw sap)
      (setf (sap self) nil)
      (setf (sap (db-opts self)) (rocksdb:rocksdb-options-destroy (sap (db-opts self)))))))

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

(defmethod multi-get ((self rdb) keys &key (data-type 'octet-vector) (opts (rocksdb-readoptions-create)) cf)
  (if cf
      (ecase data-type
        (octet-vector (multi-get-cf-kv-raw (sap self) keys opts (sap cf)))
        (string (multi-get-cf-kv-str-raw (sap self) keys opts (sap cf))))
      (ecase data-type
        (octet-vector (multi-get-kv-raw (sap self) keys opts))
        (string (multi-get-kv-str-raw (sap self) keys opts)))))

(defmethod get-value ((self rdb) key)
  (get-kv-raw (sap self) key (rocksdb-readoptions-create)))

(defmethod merge-key ((self rdb) key val &key (opts (rocksdb-writeoptions-create)))
  (merge-kv-raw (sap self) key val opts))

(defmethod merge-key ((self rdb) (key string) (val string) &key (opts (rocksdb-writeoptions-create)))
  (merge-kv-str-raw (sap self) key val opts))

(defmethod merge-kv ((self rdb) kv &key (opts (rocksdb-writeoptions-create)))
  (merge-kv-raw (sap self) (kv-key kv) (kv-val kv) opts))

;;; Transaction DB
(defstruct rdb-transaction-db 
  (name "" :type string)
  (db-opts (default-rdb-opts) :type rdb-opts)
  (sap nil :type (or null (alien (* rocksdb-transactiondb))))
  ;; struct wrapper?
  (opts (rocksdb-transactiondb-options-create)))

(defaccessor sap ((self rdb-transaction-db)) (rdb-transaction-db-sap self))
(defaccessor db-opts ((self rdb-transaction-db)) (rdb-transaction-db-db-opts self))
(defaccessor name ((self rdb-transaction-db)) (rdb-transaction-db-name self))
(defaccessor db ((self rdb-transaction-db)) (sap self))

(defmethod iter ((self rdb-transaction-db) &key cf (opts (rocksdb-readoptions-create)))
  (let ((col (etypecase cf
               (rdb-cf (rdb-cf-sap cf))
               ;; (string (rdb-cf-sap (find-column cf self)))
               (null nil)
               (alien cf))))
    (unless-null-db () self
      (make-rdb-iter 
       :sap (if col
                (transactiondb-create-iter-cf-raw sap col opts)
                (transactiondb-create-iter-raw sap opts))))))

(defstruct rdb-optimistic-transaction-db
  (name "" :type string)
  (db-opts (default-rdb-opts) :type rdb-opts)
  (sap nil :type (or null (alien (* rocksdb-optimistictransactiondb)))))

(defaccessor sap ((self rdb-optimistic-transaction-db)) (rdb-optimistic-transaction-db-sap self))
(defaccessor db-opts ((self rdb-optimistic-transaction-db)) (rdb-optimistic-transaction-db-db-opts self))
(defaccessor name ((self rdb-optimistic-transaction-db)) (rdb-optimistic-transaction-db-name self))
(defaccessor db ((self rdb-optimistic-transaction-db)) (sap self))

(defaccessor* db-opt
    ((self rdb-transaction-db) key) (db-opt (db-opts self) (string-downcase key))
    (new (self rdb-transaction-db) key &key push)
  (prog1 (setf (db-opt (db-opts self) (string-downcase key)) new)
    (when push (push-sap (db-opts self) (string-downcase key)))))

(defmethod push-opts ((self rdb-transaction-db))
  (with-slots (db-opts) self
    (push-sap* db-opts)))

(defaccessor* db-opt
    ((self rdb-optimistic-transaction-db) key) (db-opt (db-opts self) (string-downcase key))
    (new (self rdb-optimistic-transaction-db) key &key push)
  (prog1 (setf (db-opt (db-opts self) (string-downcase key)) new)
    (when push (push-sap (db-opts self) (string-downcase key)))))

(defmethod push-opts ((self rdb-optimistic-transaction-db))
  (with-slots (db-opts) self
    (push-sap* db-opts)))

(defmethod open-db ((self rdb-transaction-db))
  (with-slots (name sap opts db-opts) self
    (if sap
        (progn
          (cerror "Ignore and continue" 'open-db-error 
                  :db sap
                  :message "Database is already open")
          sap)
        (setf sap (open-transactiondb-raw (or (sap db-opts) (push-opts self)) opts name)))))

(defmethod open-db ((self rdb-optimistic-transaction-db))
  (with-slots (name sap db-opts) self
    (if sap
        (progn
          (cerror "Ignore and continue" 'open-db-error 
                  :db sap
                  :message "Database is already open")
          sap)
        (setf sap (open-optimistictransactiondb-raw (or (sap db-opts) (push-opts self)) name)))))

(defmethod close-db ((self rdb-transaction-db) &key)
  (when-let ((sap (sap self)))
    (rocksdb-transactiondb-close sap)))

(defmethod close-db ((self rdb-optimistic-transaction-db) &key)
  (when-let ((sap (sap self)))
    (rocksdb-optimistictransactiondb-close sap)))

(defmethods get-val
  (((self rdb-transaction-db) (key string) &key opts cf pinned)
   (let ((sap (sap self))
         (opts (or opts (rocksdb-readoptions-create))))
     (if cf
         (transactiondb-get-cf-str-raw sap (rdb-cf-sap (find-column cf self)) key opts pinned)
         (transactiondb-get-kv-str-raw sap key opts pinned))))
  (((self rdb-optimistic-transaction-db) (key string) &key opts cf pinned)
   (let ((sap (sap self))
         (opts (or opts (rocksdb-readoptions-create))))
     (if cf
         (transactiondb-get-cf-str-raw sap (rdb-cf-sap (find-column cf self)) key opts pinned)
         (transactiondb-get-kv-str-raw sap key opts pinned))))
  (((self rdb) key &key opts cf pinned)
   (let ((opts (or opts (rocksdb-readoptions-create))))
     (with-slots (sap) self
       (etypecase cf
         (rdb-cf (get-cf-raw sap (sap cf) key opts pinned))
         (null (get-kv-raw sap key opts pinned))
         (alien (get-cf-raw sap cf key opts pinned))))))
  (((self rdb) (key string) &key opts cf pinned)
   (octets-to-string (get-val self (string-to-octets key) :opts (or opts (rocksdb-readoptions-create)) :cf cf :pinned pinned))))

(defmethod get-value ((self rdb-transaction-db) key)
  (transactiondb-get-kv-raw self key))

;;; Transaction
(defstruct rdb-transaction 
  (sap nil :type (or null (alien (* rocksdb-transaction)))))

(defaccessor sap ((self rdb-transaction)) (rdb-transaction-sap self))
(defaccessor name ((self rdb-transaction)) (transaction-name-raw (sap self)))
(defmethod free ((self rdb-transaction)) (rocksdb-transaction-destroy (sap self)))

(defmethod transaction-object-p ((self rdb-transaction)) t)

(defmethods make-transaction 
  (((self rdb-transaction-db)
    &key name
    txn
    opts
    write-opts)
   (let ((opts (or opts (rocksdb-transaction-options-create)))
         (write-opts (or write-opts (rocksdb-writeoptions-create))))
   (let ((obj (make-rdb-transaction
               :sap (rocksdb-transaction-begin (sap self) write-opts opts txn))))
     (when name (setf (name obj) name))
     obj)))
  (((self rdb-optimistic-transaction-db)
    &key name
    txn
    opts
    write-opts)
   (let ((opts (or opts (alien-sap (rocksdb-transaction-options-create))))
         (write-opts (or write-opts (rocksdb-writeoptions-create))))
     (let ((obj (make-rdb-transaction
                 :sap (rocksdb-optimistictransaction-begin (sap self) write-opts opts txn))))
       (when name (setf (name obj) name))
       obj))))

(defmethod prepare-transaction ((self rdb-transaction) &key)
  (prepare-transaction-raw (sap self)))

(defmethod rollback-transaction ((self rdb-transaction) &key savepoint)
  (rollback-transaction-raw (sap self) savepoint))

(defmethod abort-transaction ((self rdb-transaction) &key)
  (rollback-transaction self)
  (rocksdb-transaction-destroy (sap self)))

(defmethod commit-transaction ((self rdb-transaction) &key)
  (commit-transaction-raw (sap self)))

(defun rdb-transaction-wbwi (self)
  (rocksdb-transaction-get-writebach-wi (sap self)))

(defmethod iter ((self rdb-transaction) &key cf (opts (rocksdb-readoptions-create)))
  (let ((col (etypecase cf
               (rdb-cf (rdb-cf-sap cf))
               ;; (string (rdb-cf-sap (find-column cf self)))
               (null nil)
               (alien cf))))
    (unless-null-db () self
      (make-rdb-iter 
       :sap (if col
                (transaction-create-iter-cf-raw sap col opts)
                (transaction-create-iter-raw sap opts))))))

;;; Secondary DB
(defstruct rdb-secondary-db 
  (sap nil :type (or null (alien (* rocksdb))))
  opts)

(defaccessor sap ((self rdb-secondary-db)) (rdb-secondary-db-sap self))
(defaccessor db-opts ((self rdb-secondary-db)) (rdb-secondary-db-opts self))

(defmethod open-secondary-db ((self rdb) &key path opts)
  (make-rdb-secondary-db 
   :sap (open-db-secondary-raw opts (name self) path)
   :opts opts))

(defmethod close-secondary-db ((self rdb-secondary-db))
  (rocksdb-close (sap self)))

;;; Backup DB
(defstruct rdb-backup-engine
  (sap nil :type (or null (alien (* rocksdb-backup-engine))))
  opts)

(defaccessor sap ((self rdb-backup-engine)) (rdb-backup-engine-sap self))
(defaccessor db-opts ((self rdb-backup-engine)) (rdb-backup-engine-opts self))

(defmethod open-backup-engine ((self rdb-backup-engine) &key path)
  (setf (sap self) (open-backup-engine-raw path (db-opts self))))

(defmethod close-backup-engine ((self rdb-backup-engine))
  (close-backup-engine-raw (sap self)))

(defun rdb-backup-engine-info (be)
  (etypecase be
    (rdb-backup-engine (rocksdb-backup-engine-get-backup-info (sap be)))
    (alien (rocksdb-backup-engine-get-backup-info be))))

;;; Write Batches
(defstruct rdb-writebatch 
  (sap nil :type (or null (alien (* rocksdb-writebatch)))))

(defaccessor sap ((self rdb-writebatch)) (rdb-writebatch-sap self))
(defmethod iter ((self rdb-writebatch) &key)
  (rocksdb-writebatch-iterate (sap self) nil nil (alien-callable-function 'rocksdb-delete-value)))
(defun rdb-writebatch-data (wb &optional size)
  (rocksdb-writebatch-data wb size))

;; WBWIs consist of a WriteBatch and an Index
(defstruct rdb-wbwi ;; wb reserved overwrite-key data savepoints params
  (sap (create-wbwi) :type (or null (alien (* rocksdb-writebatch-wi)))))

(defaccessor sap ((self rdb-wbwi)) (rdb-wbwi-sap self))
(defun rdb-wbwi-count (self) (rocksdb-writebatch-wi-count (sap self)))
(defun rdb-wbwi-data (wbwi &optional size)
  (rocksdb-writebatch-wi-data (sap wbwi) size))
(defmethod iter ((self rdb-wbwi) &key)
  (rocksdb-writebatch-wi-iterate (sap self) nil nil (sb-alien:alien-callable-function 'rocksdb-delete-value)))
(defun rdb-wbwi-clear (wbwi)
  (rocksdb-writebatch-wi-clear (sap wbwi)))
(defun rdb-wbwi-save (self)
  (rocksdb-writebatch-wi-set-save-point self))
(defun rdb-wbwi-ts (self ts)
  (with-errptr e
    (rocksdb-writebatch-wi-update-timestamps 
     (sap self) (octets-to-alien ts) (length ts) nil nil e)))
(defmethod destroy-db ((self rdb-wbwi))
  (setf (sap self) (rocksdb-writebatch-wi-destroy (sap self))))
(defmethod put-key ((self rdb-wbwi) (key vector) (val vector))
  (rocksdb-writebatch-wi-put 
   (sap self) 
   (cast (octets-to-alien key) (array unsigned-char))
   (length key) 
   (cast (octets-to-alien val) (array unsigned-char))
   (length val)))
(defmethod put-key ((self rdb-wbwi) (key string) (val string))
  (put-key self (string-to-octets key) (string-to-octets val)))
(defmethod put-kv ((self rdb-wbwi) (kv kv))
  (put-key self (kv-key kv) (kv-val kv)))
(defmethod get-key ((self rdb-wbwi) (key string) &key)
  (with-errptr e
    (with-alien ((i size-t))      
      (std:clone-octets-from-alien 
       (rocksdb-writebatch-wi-get-from-batch 
        (sap self) 
        (default-rocksdb-options)
        (cast (octets-to-alien (string-to-octets key)) (array unsigned-char))
        (length key)
        (addr i)
        e)
       (make-octets i)))))

(defun rdb-write (db batch &optional opts)
  (with-errptr e (rocksdb-write-writebatch-wi (sap db) (sap (or opts (make-rdb-writeopts))) (sap batch) e)))

(defun wbwi-put-kv-cf (wbwi column kv)
  (wbwi-put-cf-raw (sap wbwi) (sap column) (kv-key kv) (kv-val kv)))

;;; Env
(defstruct rdb-env 
  (sap nil :type (or null (alien (* rocksdb-env))))
  path 
  threads)

(defaccessor sap ((self rdb-env)) (rdb-env-sap self))
(defaccessor path ((self rdb-env)) (rdb-env-path self))
(defmethod free ((self rdb-env)) (rocksdb-env-destroy (sap self)))

;;; Logger
(defun rdb-log-default (level &optional prefix)
  (if prefix
      (rocksdb-logger-create-stderr-logger level prefix)
      (rocksdb-logger-create-callback-logger 
       level 
       (alien-sap (alien-callable-function 'rocksdb-log-default)) 
       nil)))
