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

(defun get-metadata-raw (db &optional cf)
  (if cf
      (rocksdb-get-column-family-metadata-cf db cf)
      (rocksdb-get-column-family-metadata db)))

(defun flush-db-raw (db &optional wait)
  (with-errptr* (err 'flush-db-error :db db)
    (let ((opts (rocksdb-flushoptions-create)))
      (when wait (rocksdb-flushoptions-set-wait opts wait))
      (rocksdb-flush db opts err))))

(defun repair-db-raw (name &optional (opts (rocksdb-options-create)))
  (with-errptr* (err 'repair-db-error :name name)
    (rocksdb-repair-db opts name err)))

(defun ingest-db-raw (db files &optional (opts (rocksdb-ingestexternalfileoptions-create)))
  (let ((flen (length files)))
    (with-errptr* (err 'ingest-db-error)
      (with-alien ((flist (* c-string) (make-alien c-string flen)))
        (loop for f in files
              for i from 0 to flen
              do (setf (deref flist i) (make-alien-string f :null-terminate t)))
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
    (with-kv-raw (db key e :error put-kv-error :val val)
      (rocksdb-put db opts
		   %key %klen
		   %val %vlen
		   e)))

(defun put-kv-str-raw (db key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (put-kv-raw db key-octets val-octets opts)))

(defun get-kv-raw (db key &optional (opt (rocksdb-readoptions-create)) pinned)
  (with-kv-raw (db key e :error get-kv-error)
      (with-alien ((vlen size-t))
        (let* ((val (if pinned 
                        (rocksdb-get-pinned db opt %key %klen e)
                        (rocksdb-get db
                                     opt
                                     %key
                                     %klen
                                     (addr vlen)
                                     e))))
	  ;; helps if we know the vlen beforehand, would need a custom
	  ;; C-side function probably.
          (let ((v (make-octets vlen)))
            (clone-octets-from-alien val v vlen)
            (coerce v 'octet-vector))))))

(defun get-kv-str-raw (db key &optional (opt (rocksdb-readoptions-create)) pinned)
  (let ((k (string-to-octets key)))
    (let ((v (get-kv-raw db k opt pinned)))
      (when v (octets-to-string v)))))

(defun multi-get-kv-raw (db keys &optional (opt (rocksdb-readoptions-create)))
  (multiple-value-bind (keys keyns) (clone-octet-vector-list* keys)
    (let ((n (length keys)))
      (with-alien ((vals (* (* (unsigned 8))) (make-alien (* (unsigned 8)) n))
                   (valns (* size-t) (make-alien size-t n))
                   (errs (* rocksdb-errptr) (make-alien rocksdb-errptr n)))        
        (rocksdb-multi-get db opt n keys keyns vals valns errs)))))

(defun multi-get-kv-str-raw (db keys &optional (opt (rocksdb-readoptions-create)))
  (let ((n (length keys))
        (keys (clone-strings keys nil))
        (keyns (clone-integer-list (mapcar 'length keys))))
    (with-alien ((vals (* c-string) (make-alien c-string n))
                 (valns (* size-t) (make-alien size-t n))
                 (errs (* rocksdb-errptr) (make-alien rocksdb-errptr n)))
      (rocksdb-multi-get db opt n keys keyns vals valns errs))))

(defun multi-get-cf-kv-raw (db cfs keys &optional (opt (rocksdb-readoptions-create)))
  (multiple-value-bind (keys keyns) (clone-octet-vector-list* keys)
    (let ((n (length keys)))
      (with-alien ((%cfs (* (* rocksdb-column-family-handle)) (make-alien (* rocksdb-column-family-handle) n))
                   (vals (* (* (unsigned 8))) (make-alien (* (unsigned 8)) n))
                   (valns (* size-t) (make-alien size-t n))
                   (errs (* rocksdb-errptr) (make-alien rocksdb-errptr n)))
        (loop for i below n do (setf (deref %cfs i) (pop cfs)))
        (rocksdb-multi-get-cf db opt cfs n keys keyns vals valns errs)))))

(defun multi-get-cf-kv-str-raw (db cfs keys &optional (opt (rocksdb-readoptions-create)))
  (let ((n (length keys))
        (keys (clone-strings keys nil))
        (keyns (clone-integer-list (mapcar 'length keys))))
    (with-alien ((%cfs (* (* rocksdb-column-family-handle)) (make-alien (* rocksdb-column-family-handle) n))
                 (vals (* c-string) (make-alien c-string n))
                 (valns (* size-t) (make-alien size-t n))
                 (errs (* rocksdb-errptr) (make-alien rocksdb-errptr n)))
      (loop for i below n do (setf (deref %cfs i) (pop cfs)))
      (rocksdb-multi-get-cf db opt cfs n keys keyns vals valns errs))))

(defun merge-kv-raw (db key val &optional (opt (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :error merge-kv-error :val val)
    (rocksdb-merge db opt %key %klen %val %vlen e)))

(defun merge-kv-str-raw (db key val &optional (opt (rocksdb-writeoptions-create)))
  (let ((k (string-to-octets key))
        (v (string-to-octets val)))
    (merge-kv-raw db k v opt)))

;;; Column Family
(defun open-cfs-raw (db-opt name names opts)
  (let ((n (length names)))
    (with-alien ((cf-names (* c-string) (clone-strings names))
                 (cf-opts (* (* rocksdb-options)) (make-alien (* rocksdb-options) n))
                 (cf-handles (* (* rocksdb-column-family-handle)) (make-alien (* rocksdb-column-family-handle) n)))
      (loop for opt in opts
            for i below n
            do (setf (deref cf-opts i) opt))
      (with-errptr* (err 'cf-error :cf name)
        (let ((db (rocksdb-open-column-families db-opt name n cf-names cf-opts cf-handles err)))
          (values db cf-handles))))))

(defun create-cf-raw (db name &optional (opt (rocksdb-options-create)))
  (with-errptr* (err 'cf-error :db db :cf name)
    (rocksdb-create-column-family db opt name err)))

(defun destroy-cf-raw (cf)
  (rocksdb-column-family-handle-destroy cf))

(defun get-cf-raw (db cf key &optional (opt (rocksdb-readoptions-create)) pinned)
  (with-kv-raw (db key e :error get-kv-cf-error :cf cf)
      (with-alien ((vlen (* size-t) (make-alien size-t)))
        (let ((val (if pinned
                       (rocksdb-get-pinned db opt %key %klen e)
                       (rocksdb-get-cf db
			               opt
                                       cf
			               %key 
                                       %klen
                                       vlen
			               e)))
	      ;; helps if we know the vlen beforehand, would need a custom
	      ;; C-side function probably.
	      (v (make-array (deref vlen) :element-type 'octet)))
          (clone-octets-from-alien val v (deref vlen))
	  (print v)))))

(defun get-cf-str-raw (db cf key &optional (opt (rocksdb-readoptions-create)) pinned)
  (let ((k (string-to-octets key :null-terminate nil)))
    (let ((v (get-cf-raw db cf k opt pinned)))
      (when v (octets-to-string v)))))

(defun put-cf-raw (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :error put-kv-cf-error :val val :cf cf)
    (rocksdb-put-cf db
                    opts
                    cf
                    %key %klen
                    %val %vlen
                    e)))

(defun put-cf-str-raw (db cf key val &optional (opt (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (put-cf-raw db cf key-octets val-octets opt)))

(defun merge-cf-raw (db cf key val &optional (opt (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :cf cf :error merge-kv-error :val val)
    (rocksdb-merge-cf db opt cf %key %klen %val %vlen e)))

(defun merge-cf-str-raw (db cf key val &optional (opt (rocksdb-writeoptions-create)))
  (let ((k (string-to-octets key))
        (v (string-to-octets val)))
    (merge-cf-raw db cf k v opt)))

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

;;; Backup DB
(defun open-backup-engine-raw (be-path &optional (opts (rocksdb-backup-engine-options-create)))
  (with-errptr* (err 'open-backup-db-error :db be-path)
    (let ((be-path (if (pathnamep be-path)
                       (namestring be-path)
                       be-path)))
      (rocksdb-backup-engine-options-set-backup-dir opts be-path)
      (rocksdb-backup-engine-open opts be-path err))))

(defun close-backup-engine-raw (be)
  (rocksdb-backup-engine-close be))

(defun create-new-backup-raw (be db)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-backup-engine-create-new-backup be db err)))

(defun restore-from-latest-backup-raw (be db-path backup-path &optional (opt (rocksdb-restore-options-create)))
  (with-errptr* (err 'open-db-error)
    (rocksdb-backup-engine-restore-db-from-latest-backup be db-path backup-path opt err)))

(defun restore-from-backup-raw (be db-path backup-path backup-id &optional (opt (rocksdb-restore-options-create)))
  (with-errptr* (err 'open-db-error)
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

;;; Transactions
(defun open-transactiondb-raw (opts topts name)
  (with-errptr* (e 'open-db-error :db name)
    (rocksdb-transactiondb-open opts topts name e)))

(defun open-optimistictransactiondb-raw (opts name)
  (with-errptr* (e 'open-db-error :db name)
    (rocksdb-optimistictransactiondb-open opts name e)))

(defun transactiondb-get-kv-raw (db key &optional (opts (rocksdb-readoptions-create)) pinned)
  (with-kv-raw (db key e :error get-kv-error)
    (with-alien ((vlen (* size-t)))
      (let* ((val (if pinned
                      (rocksdb-transactiondb-get-pinned db opts %key %klen e)
                      (rocksdb-transactiondb-get db opts %key %klen vlen e)))
             (v (make-array (deref vlen) :element-type 'octet)))
        (clone-octets-from-alien val v (deref vlen))
        v))))

(defun transactiondb-get-kv-str-raw (db key &optional (opts (rocksdb-readoptions-create)) pinned)
  (let ((k (string-to-octets key)))
    (let ((v (transactiondb-get-kv-raw db k opts pinned)))
      (when v (octets-to-string v)))))

(defun transactiondb-get-cf-raw (db cf key &optional (opts (rocksdb-readoptions-create)) pinned)
  (with-kv-raw (db key e :error get-kv-cf-error :cf cf)
    (with-alien ((vlen (* size-t)))
      (let* ((val (if pinned
                      (rocksdb-transactiondb-get-pinned-cf db opts cf %key %klen e)
                      (rocksdb-transactiondb-get-cf db opts cf %key %klen vlen e)))
             (v (make-array (deref vlen) :element-type 'octet)))
        (clone-octets-from-alien val v (deref vlen))
        v))))

(defun transactiondb-get-cf-str-raw (db cf key &optional (opts (rocksdb-readoptions-create)) pinned)
  (let ((k (string-to-octets key)))
    (let ((v (transactiondb-get-cf-raw db cf k opts pinned)))
      (when v (octets-to-string v)))))

(defun transactiondb-put-kv-raw (db key val &optional (opts (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :error put-kv-error :val val)
    (rocksdb-transactiondb-put db opts %key %klen %val %vlen e)))

(defun transactiondb-put-kv-str-raw (db key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (transactiondb-put-kv-raw db key-octets val-octets opts)))

(defun transactiondb-put-cf-raw (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :error put-kv-cf-error :val val :cf cf)
    (rocksdb-transactiondb-put-cf db
                                  opts
                                  cf
                                  %key %klen
                                  %val %vlen
                                  e)))

(defun transactiondb-put-cf-str-raw (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (transactiondb-put-cf-raw db cf key-octets val-octets opts)))

(defun commit-transaction-raw (txn)
  (with-errptr* (e 'rdb-alien-error)
    (rocksdb-transaction-commit txn e)))

(defun rollback-transaction-raw (txn &optional savepoint)
  "Rollback a raw transaction TXN when SAVEPOINT is non-nil only rollback to last
savepoint created with ROCKSDB-TRANSACTION-SET-SAVEPOINT."
  (with-errptr* (e 'rdb-alien-error)
    (if savepoint
        (rocksdb-transaction-rollback-to-savepoint txn e)
        (rocksdb-transaction-rollback txn e))))

(defun prepare-transaction-raw (txn)
  (with-errptr* (e 'rdb-alien-error)
    (rocksdb-transaction-prepare txn e)))

(defun transaction-name-raw (txn)
  (with-errptr* (e 'rdb-alien-error)
    (with-alien ((len size-t))
      (let ((name (rocksdb-transaction-get-name txn (addr len)))
            (ret (make-octets len)))
        (octets-to-string (clone-octets-from-alien name ret len))))))

(defun set-transaction-name-raw (txn name)
  (with-errptr* (e 'rdb-alien-error)
    (let ((nlen (length name)))
      (with-alien ((%name (* unsigned-char) (octets-to-alien (string-to-octets name))))
        (rocksdb-transaction-set-name txn %name nlen e)))))

(defsetf transaction-name-raw set-transaction-name-raw)

(defun transaction-get-raw (txn key &optional (opts (rocksdb-readoptions-create)) pinned)
  (with-txn-raw (txn e :key key)
    (with-alien ((vlen size-t))
      (if pinned
          (rocksdb-transaction-get-pinned txn opts %key %klen e)
          (rocksdb-transaction-get txn opts %key %klen (addr vlen) e)))))

(defun transaction-get-cf-raw (txn cf key &optional (opts (rocksdb-readoptions-create)) pinned)
  (with-txn-raw (txn e :key key :cf cf)
    (with-alien ((vlen size-t))
      (if pinned
          (rocksdb-transaction-get-pinned-cf txn opts cf %key %klen e)
          (rocksdb-transaction-get-cf txn opts cf %key %klen (addr vlen) e)))))

(defun transaction-delete-raw (txn key)
  (with-txn-raw (txn e :key key)
    (rocksdb-transaction-delete txn %key %klen e)))

(defun transaction-delete-cf-raw (txn cf key)
  (with-txn-raw (txn e :key key :cf cf)
    (rocksdb-transaction-delete-cf txn cf %key %klen e)))

(defun transaction-put-raw (txn key val)
  (with-txn-raw (txn e :key key :val val)
    (rocksdb-transaction-put txn %key %klen %val %vlen e)))

(defun transaction-put-cf-raw (txn cf key val)
  (with-txn-raw (txn e :cf cf :key key :val val)
    (rocksdb-transaction-put-cf txn cf %key %klen %val %vlen e)))

(defun transaction-merge-raw (txn key val)
  (with-txn-raw (txn e :key key :val val)
    (rocksdb-transaction-merge txn %key %klen %val %vlen e)))

(defun transaction-merge-cf-raw (txn cf key val)
  (with-txn-raw (txn e :key key :val val :cf cf)
    (rocksdb-transaction-merge-cf txn cf %key %klen %val %vlen e)))

(defun get-prepared-transactions-raw (txn-db)
  "Return an array of prepared ROCKSDB-TRANSACTION pointers from this
transaction-db."
  (with-errptr* (e 'rdb-alien-error)
    (with-alien ((cnt size-t))
      (rocksdb-transactiondb-get-prepared-transactions txn-db (addr cnt)))))

;;; Checkpoints
(defun make-checkpoint-raw (db)
  (with-errptr* (e 'rdb-alien-error)
    (rocksdb-checkpoint-object-create db e)))

(defun create-checkpoint-raw (chk dir &optional log-size-for-flush)
  (with-errptr* (e 'rdb-alien-error)
    (rocksdb-checkpoint-create chk dir log-size-for-flush e)))

;;; Secondary
(defun open-db-secondary-raw (opts name sname)
  (with-errptr* (e 'rdb-alien-error)
    (rocksdb-open-as-secondary opts name sname e)))

(defun open-cfs-secondary-raw (opts name sname cf-names cf-opts)
  (with-errptr* (e 'rdb-alien-error)
    (with-alien ((cf-handles (array (* rocksdb-column-family-handle))))
      (rocksdb-open-as-secondary-column-families 
       opts name sname (length cf-names) cf-names cf-opts cf-handles e))))

;;; Read-only
(defun open-cfs-read-only-raw (opts name cf-names cf-opts &optional err-if-wal)
  (with-errptr* (e 'rdb-alien-error)
    (with-alien ((cf-handles (array (* rocksdb-column-family-handle))))
      (rocksdb-open-for-read-only-column-families 
       opts name (length cf-names) cf-names cf-opts cf-handles err-if-wal e))))

;;; TTL
(defun open-cfs-with-ttl-raw (opts name cf-names cf-opts ttls)
    (with-errptr* (e 'rdb-alien-error)
      (with-alien ((cf-handles (array (* rocksdb-column-family-handle))))
        (rocksdb-open-column-families-with-ttl 
         opts name (length cf-names) cf-names cf-opts cf-handles ttls e))))
;;; Merge Ops
(defun create-index-merge-op ()
  (with-alien ((state (* t))
               (destructor (* rocksdb-destructor-function) (alien-sap (alien-callable-function 'rocksdb-destructor)))
               (full-merge (* rocksdb-full-merge-function) (alien-sap (alien-callable-function 'rocksdb-index-full-merge)))
               (partial-merge (* rocksdb-partial-merge-function) (alien-sap (alien-callable-function 'rocksdb-index-partial-merge)))
               (delete-value (* rocksdb-delete-value-function) (alien-sap (alien-callable-function 'rocksdb-delete-value)))
               (name (* rocksdb-name-function) (alien-sap (alien-callable-function 'rocksdb-index-merge-name))))
    (rocksdb-mergeoperator-create state destructor full-merge partial-merge delete-value name)))

(defun create-concat-merge-op ()
  ;; concat merge op
  (with-alien ((state (* t))
               (destructor (* rocksdb-destructor-function) (alien-sap (alien-callable-function 'rocksdb-destructor)))
               (full-merge (* rocksdb-full-merge-function) (alien-sap (alien-callable-function 'rocksdb-concat-full-merge)))
               (partial-merge (* rocksdb-partial-merge-function) (alien-sap (alien-callable-function 'rocksdb-concat-partial-merge)))
               (delete-value (* rocksdb-delete-value-function) (alien-sap (alien-callable-function 'rocksdb-delete-value)))
               (name (* rocksdb-name-function) (alien-sap (alien-callable-function 'rocksdb-concat-merge-name))))
    (rocksdb-mergeoperator-create state destructor full-merge partial-merge delete-value name)))

;;; Prefix Ops
(defun create-fixed-prefix-op (n)
  (rocksdb-slicetransform-create-fixed-prefix n))

;;; Logger
(defun create-default-logger-callback (&optional (level 0))
  (rocksdb-logger-create-callback-logger 
   level 
   (alien-sap (alien-callable-function 'rocksdb-log-default)) nil))

;;; Writebatch/WBWI
(defun create-wbwi (&optional (reserved-bytes 0) (overwrite-keys 1))
  (rocksdb-writebatch-wi-create reserved-bytes overwrite-keys))
