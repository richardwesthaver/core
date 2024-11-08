;;; rdb/raw.lisp --- Intermediate API to ROCKSDB aliens

;;; Code:
(in-package :rdb)

;;; Options
(defun make-rocksdb-options (&optional init-fn)
  "Make and return RDB-OPTS. INIT-FN is an optional argument which must be a
lambda which takes a single parameter (the RDB-OPTS sap). It is used
to initialize the instance with custom configuration."
  (let ((opts (rocksdb-options-create)))
    (when init-fn (funcall init-fn opts))
    opts))

(defun default-rocksdb-options ()
  (make-rocksdb-options
   (lambda (o) (rocksdb-options-set-create-if-missing o t))))

(defun load-opts-raw (dir)
  (rocksdb::with-latest-options dir (db-opts names cf-opts)
    (values db-opts names cf-opts)))
    
(defun get-stats-raw (opt htype)
  (with-alien ((hist (* rocksdb-statistics-histogram-data) (rocksdb-statistics-histogram-data-create)))
    (rocksdb-options-statistics-get-histogram-data opt htype hist)
    (deref hist)))

;;; DB
(defun open-db-raw (db-path &optional (opts (default-rocksdb-options)))
  (with-errptr* (err 'open-db-error :db db-path)
    (let* ((db-path (if (pathnamep db-path)
                        (namestring db-path)
                        db-path)))
      (rocksdb-open opts db-path err))))

(defun close-db-raw (db)
  (rocksdb-close db))

(defun destroy-db-raw (path &optional (opt (rocksdb-options-create)))
  (with-errptr* (err 'destroy-db-error :db path)
    (rocksdb-destroy-db opt (namestring (uiop:ensure-directory-pathname path)) err)
    (rocksdb-options-destroy opt)))

(defun get-property-raw (db str)
  (rocksdb-property-value db (make-alien-string str)))

(defun get-metadata-raw (db &optional cf)
  (if cf
      (rocksdb-get-column-family-metadata-cf db cf)
      (rocksdb-get-column-family-metadata db)))

(defun flush-db-raw (db &optional (opts (rocksdb-flushoptions-create)))
  (with-errptr* (err 'flush-db-error :db db)
    (rocksdb-flush db opts err)))

(defun repair-db-raw (name &optional (opts (rocksdb-options-create)))
  (with-errptr* (err 'repair-db-error :name name)
    (rocksdb-repair-db opts name err)))

(defun ingest-db-raw (db files &optional (opts (rocksdb-ingestexternalfileoptions-create)))
  (let ((flen (length files)))
    (with-errptr* (err 'ingest-db-error)
      (with-alien ((flist (* c-string) (make-alien c-string flen)))
        (loop for f in files
              for i from 0 to flen
              do (setf (deref flist i) (make-alien-string f :null-terminate nil)))
        (rocksdb-ingest-external-file db flist flen opts err)))))

(defun ingest-db-cf-raw (db cf files &optional (opts (rocksdb-ingestexternalfileoptions-create)))
  (let ((flen (length files)))
    (with-errptr* (err 'ingest-db-error)
      (with-alien ((flist (* c-string) (make-alien c-string flen)))
        (loop for f in files
              for i from 0 to flen
              do (setf (deref flist i) (make-alien-string f :null-terminate nil)))
        (rocksdb-ingest-external-file-cf db cf flist flen opts err)))))
  
;;; KVs
(defun put-kv-raw (db key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((klen (length key))
	(vlen (length val)))
    (with-alien ((k (* unsigned-char) (make-alien unsigned-char klen))
		 (v (* unsigned-char) (make-alien unsigned-char vlen)))
      (setfa k key)
      (setfa v val)
      (with-errptr* (err 'put-kv-error :db db :kv (cons key val))
        (rocksdb-put db
		     opts
		     k
		     klen
		     v
		     vlen
		     err)))))

(defun put-kv-str-raw (db key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (put-kv-raw db key-octets val-octets opts)))

(defun get-kv-raw (db key &optional (opt (rocksdb-readoptions-create)))
  (let ((klen (length key)))
    (with-errptr* (err 'get-kv-error :db db :key key)
      (with-alien ((vlen size-t)
		   (k (* unsigned-char) (make-alien unsigned-char klen)))
        (setfa k key)
        (let* ((val (rocksdb-get db
			         opt
			         k klen
                                 (addr vlen)
			         err)))
	  ;; helps if we know the vlen beforehand, would need a custom
	  ;; C-side function probably.
          (let ((v (make-array vlen :element-type 'octet)))
            (clone-octets-from-alien val v vlen)
            (coerce v 'octet-vector)))))))

(defun get-kv-str-raw (db key &optional (opt (rocksdb-readoptions-create)))
  (let ((k (string-to-octets key)))
    (let ((v (get-kv-raw db k opt)))
      (when v (octets-to-string v)))))

;;; Column Family
(defun open-cfs-raw (db-opt name names opts)
  (let ((n (length names)))
    (with-alien ((cf-names (* c-string) (clone-strings names))
                 (cf-opts (* (* rocksdb-options)))
                 (cf-handles (* (* rocksdb-column-family-handle))))
      (loop for opt in opts
            for i below n
            do (setf (deref cf-opts i) opt))
      (with-errptr* (err 'rocksdb-cf-error :cf name)
        (let ((db (rocksdb-open-column-families db-opt name n cf-names cf-opts cf-handles err)))
          (values db cf-handles))))))

(defun create-cf-raw (db name &optional (opt (rocksdb-options-create)))
  (with-errptr* (err 'rocksdb-cf-error :db db :cf name)
    (rocksdb-create-column-family db opt name err)))

(defun destroy-cf-raw (cf)
  (rocksdb-column-family-handle-destroy cf))

(defun get-cf-raw (db cf key &optional (opt (rocksdb-readoptions-create)))
  (let ((klen (length key)))
    (with-errptr* (err 'get-kv-error :db db :key key)
      (with-alien ((vlen (* size-t) (make-alien size-t 0))
		   (k (* unsigned-char) (make-alien unsigned-char klen)))
        (setfa k key)
        (let* ((val (rocksdb-get-cf db
			            opt
                                    cf
			            k klen
                                    vlen
			            err)))
	  ;; helps if we know the vlen beforehand, would need a custom
	  ;; C-side function probably.
	  (let ((v (make-array (deref vlen) :element-type 'octet)))
            (clone-octets-from-alien val v (deref vlen))
	    v))))))

(defun get-cf-str-raw (db cf key &optional (opt (rocksdb-readoptions-create)))
  (let ((k (string-to-octets key :null-terminate nil)))
    (let ((v (get-cf-raw db cf k opt)))
      (when v (octets-to-string v)))))

(defun put-cf-raw (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((klen (length key))
        (vlen (length val)))
    (with-errptr* (err 'put-kv-error :db db :kv (cons key val))
      (with-alien ((k (* unsigned-char) (make-alien unsigned-char klen))
                   (v (* unsigned-char) (make-alien unsigned-char vlen)))
        (setfa k key)
        (setfa v val)
        (rocksdb-put-cf db
                        opts
                        cf
                        k klen
                        v vlen
                        err)))))

(defun put-cf-str-raw (db cf key val &optional (opt (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (put-cf-raw db cf key-octets val-octets opt)))

(defun cf-name-raw (cf-handle)
  (rocksdb-column-family-handle-get-name cf-handle (make-alien unsigned-long)))

(defun cf-id-raw (cf-handle)
  (rocksdb-column-family-handle-get-id cf-handle))

;;; Iterators
(defun create-iter-raw (db &optional (opt (rocksdb-readoptions-create)))
  (rocksdb-create-iterator db opt))

(defun create-cf-iter-raw (db cf &optional (opt (rocksdb-readoptions-create)))
  (rocksdb-create-iterator-cf db opt cf))

(defun destroy-iter-raw (iter)
  (rocksdb-iter-destroy iter))

(defun iter-key-raw (iter)
  (with-alien ((klen-ptr (* size-t) (make-alien size-t 0)))
    (let* ((key-ptr (rocksdb-iter-key iter klen-ptr))
           (klen (deref klen-ptr))
           (k (make-array klen :element-type '(unsigned-byte 8))))
      (clone-octets-from-alien key-ptr k klen)
      k)))

(defun iter-key-str-raw (iter)
  (when-let ((k (iter-key-raw iter)))
    (octets-to-string k)))

(defun iter-val-raw (iter)
  (with-alien ((vlen-ptr (* size-t) (make-alien size-t 0)))
    (let* ((val-ptr (rocksdb-iter-value iter vlen-ptr))
           (vlen (deref vlen-ptr))
           (v (make-array vlen :element-type '(unsigned-byte 8))))
      (clone-octets-from-alien val-ptr v vlen)
      v)))

(defun iter-val-str-raw (iter)
  (when-let ((v (iter-val-raw iter)))
    (octets-to-string v)))

;;; Backup Engine
(defun open-backup-engine-raw (be-path &optional (opts (rocksdb-options-create)))
  (with-errptr* (err 'open-backup-engine-error :db be-path)
    (let ((be-path (if (pathnamep be-path)
                       (namestring be-path)
                       be-path)))
      (rocksdb-backup-engine-open opts be-path err))))

(defun close-backup-engine-raw (be)
  (rocksdb-backup-engine-close be))

(defun create-new-backup-raw (be db)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-backup-engine-create-new-backup be db err)))

(defun restore-from-latest-backup-raw (be db-path backup-path &optional (opt (rocksdb-restore-options-create)))
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-backup-engine-restore-db-from-latest-backup be db-path backup-path opt err)))

(defun restore-from-backup-raw (be db-path backup-path backup-id &optional (opt (rocksdb-restore-options-create)))
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-backup-engine-restore-db-from-backup be db-path backup-path opt backup-id err)))

;;; Snapshot
(defun create-snapshot-raw (db)
  (rocksdb-create-snapshot db))

(defun release-snapshot-raw (db snapshot)
  (rocksdb-release-snapshot db snapshot))

;;; SST
(defun create-sst-writer-raw (&optional (env-opts (rocksdb-envoptions-create)) (io-opts (rocksdb-options-create)))
  (rocksdb-sstfilewriter-create env-opts io-opts))

(defun create-sst-writer-with-comparator-raw (comparator
                                              &optional
                                                (env-opts (rocksdb-envoptions-create))
                                                (io-opts (rocksdb-options-create)))
  (rocksdb-sstfilewriter-create-with-comparator env-opts io-opts comparator))

(defun finish-sst-writer-raw (writer)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-finish writer err)))

(defun destroy-sst-writer-raw (writer)
  (rocksdb-sstfilewriter-destroy writer))

(defun open-sst-writer-raw (writer name)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-open writer name err)))

;; this function is deprecated in the Java API:
;; https://javadoc.io/doc/org.rocksdb/rocksdbjni/6.6.4/org/rocksdb/SstFileWriter.html

;; (defun sst-add-raw (writer key val)
;;   (with-errptr* (err 'rdb-alien-error)
;;     (rocksdb-sstfilewriter-add writer key (length key) val (length val) err)))

(defun sst-put-raw (writer key val)
  (let ((klen (length key))
        (vlen (length val)))
    (with-errptr* (err 'rdb-alien-error)
      (with-alien ((k (* unsigned-char) (make-alien unsigned-char klen))
                   (v (* unsigned-char) (make-alien unsigned-char vlen)))
        (setfa k key)
        (setfa v val)
        (rocksdb-sstfilewriter-put writer k klen v vlen err)))))

(defun sst-put-str-raw (writer key val)
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (sst-put-raw writer key-octets val-octets)))

(defun sst-put-ts-raw (writer key val ts)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-put-with-ts writer key (length key) val (length val) ts (length ts) err)))

(defun sst-delete-raw (writer key)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-delete writer key (length key) err)))

(defun sst-delete-ts-raw (writer key ts)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-delete-with-ts writer key (length key) ts (length ts) err)))

(defun sst-delete-range-raw (writer start-key end-key)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-delete-range writer start-key (length start-key) end-key (length end-key) err)))

(defun sst-file-size-raw (writer)
  (with-errptr* (err 'rdb-alien-error)
    (with-alien ((ret unsigned-long))
      (rocksdb-sstfilewriter-file-size writer (addr ret) err)
      ret)))
