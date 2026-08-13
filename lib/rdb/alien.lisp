;;; rdb/alien.lisp --- Intermediate API to ROCKSDB aliens

;;; Code:
(in-package :rdb)

;;; Options
(defun %load-opts (dir)
  (rocksdb::with-latest-options dir (db-opts names cf-opts)
    (values db-opts names cf-opts)))

(defun %get-stats (opt htype)
  (with-alien ((hist (* rocksdb-statistics-histogram-data) (rocksdb-statistics-histogram-data-create)))
    (rocksdb-options-statistics-get-histogram-data opt htype hist)
    hist))

;;; DB
(defun %open-db (db-path &optional (opts (rocksdb-options-create)))
  (with-errptr* (err 'open-db-error :db db-path)
    (let* ((db-path (if (pathnamep db-path)
                        (namestring db-path)
                        db-path)))
      (rocksdb-open opts db-path err))))

(defun %close-db (db)
  (when db
    (typecase db
      ((alien (* rocksdb)) (rocksdb-close db))
      ((alien (* rocksdb-transactiondb)) (rocksdb-transactiondb-close db))
      ((alien (* rocksdb-optimistictransactiondb)) (rocksdb-optimistictransactiondb-close db)))))

(defun %destroy-db (path &optional (opt (rocksdb-options-create)))
  (with-errptr* (err 'destroy-db-error :db path)
    (rocksdb-destroy-db opt (namestring (probe-directory path)) err)
    (rocksdb-options-destroy opt)))

(defun %get-metadata (db &optional cf)
  (if cf
      (rocksdb-get-column-family-metadata-cf db cf)
      (rocksdb-get-column-family-metadata db)))

(defun %flush-db (db &optional wait)
  (with-errptr* (err 'flush-db-error :db db)
    (let ((opts (rocksdb-flushoptions-create)))
      (when wait (rocksdb-flushoptions-set-wait opts wait))
      (rocksdb-flush db opts err))))

(defun %repair-db (name &optional (opts (rocksdb-options-create)))
  (with-errptr* (err 'repair-db-error :name name)
    (rocksdb-repair-db opts name err)))

(defun %ingest-db (db files &optional (opts (rocksdb-ingestexternalfileoptions-create)))
  (let ((flen (length files)))
    (with-errptr* (err 'ingest-db-error)
      (with-alien ((flist (* c-string) (make-alien c-string flen)))
        (loop for f in files
              for i from 0 to flen
              do (setf (deref flist i) (make-alien-string f :null-terminate t)))
        (rocksdb-ingest-external-file db flist flen opts err)))))

(defun %ingest-db-cf (db cf files &optional (opts (rocksdb-ingestexternalfileoptions-create)))
  (let ((flen (length files)))
    (with-errptr* (err 'ingest-db-error)
      (with-alien ((flist (* c-string) (make-alien c-string flen)))
        (loop for f in files
              for i from 0 to flen
              do (setf (deref flist i) (make-alien-string f :null-terminate t)))
        (rocksdb-ingest-external-file-cf db cf flist flen opts err)))))
  
;;; KVs
(defun %key-exists-p (db key length &optional (opts (rocksdb-readoptions-create)) timestamp)
  (with-alien ((found boolean)
               (v (* unsigned-char))
               (vlen size-t))
    (and
     (rocksdb-key-may-exist db opts key length (addr v) (addr vlen) 
                            timestamp (if timestamp (length timestamp) 0)
                            (addr found))
     found
     (not (zerop vlen))
     (values v vlen))))

(defun %cf-key-exists-p (db cf key length &optional (opts (rocksdb-readoptions-create)) timestamp)
  (with-alien ((found boolean)
               (v (* unsigned-char))
               (vlen size-t))
    (and
     (rocksdb-key-may-exist-cf db opts cf key length (addr v) (addr vlen) 
                               timestamp (if timestamp (length timestamp) 0)
                               (addr found))
     found
     (not (zerop vlen))
     (values v vlen))))

(defun %put-kv (db key val &optional (opts (rocksdb-writeoptions-create)))
    (with-kv-raw (db key e :error put-kv-error :val val)
      (rocksdb-put db opts
		   %key %klen
		   %val %vlen
		   e)))

(defun %put-kv-str (db key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (%put-kv db key-octets val-octets opts)))

(defun %get-kv (db key &optional (opt (rocksdb-readoptions-create)) pinned)
  (with-kv-raw (db key e :error get-kv-error)
      (with-alien ((vlen size-t))
        (let* ((val (if pinned 
                        (rocksdb-get-pinned db opt %key %klen e)
                        (rocksdb-get db
                                     opt
                                     %key
                                     %klen
                                     vlen
                                     e))))
	  ;; helps if we know the vlen beforehand, would need a custom
	  ;; C-side function probably.
          (let ((v (make-octets vlen)))
            (clone-octets-from-alien val v vlen)
            (coerce v 'octet-vector))))))

(defun %get-kv-str (db key &optional (opt (rocksdb-readoptions-create)) pinned)
  (let ((k (string-to-octets key)))
    (let ((v (%get-kv db k opt pinned)))
      (when v (octets-to-string v)))))

(defun %multi-get-kv (db keys &optional (opt (rocksdb-readoptions-create)))
  (multiple-value-bind (keys keyns) (clone-octet-vector-list* keys)
    (let ((n (length keys)))
      (with-alien ((vals (* c-string) (make-alien c-string n))
                   (valns (* size-t) (make-alien size-t n))
                   (errs (* rocksdb-errptr) (make-alien rocksdb-errptr n)))     
        (rocksdb-multi-get db opt n keys keyns vals valns errs)))))

(defun %multi-get-kv-str (db keys &optional (opt (rocksdb-readoptions-create)))
  (let ((n (length keys))
        (keys (clone-strings keys nil))
        (keyns (clone-integer-list (mapcar 'length keys))))
    (with-alien ((vals (* c-string) (make-alien c-string n))
                 (valns (* size-t) (make-alien size-t n))
                 (errs (* rocksdb-errptr) (make-alien rocksdb-errptr n)))
      (rocksdb-multi-get db opt n keys keyns vals valns errs))))

(defun %multi-get-cf-kv (db cfs keys &optional (opt (rocksdb-readoptions-create)))
  (multiple-value-bind (keys keyns) (clone-octet-vector-list* keys)
    (let ((n (length keys)))
      (with-alien ((%cfs (* (* rocksdb-column-family-handle)) (make-alien (* rocksdb-column-family-handle) n))
                   (vals (* (* (unsigned 8))) (make-alien (* (unsigned 8)) n))
                   (valns (* size-t) (make-alien size-t n))
                   (errs (* rocksdb-errptr) (make-alien rocksdb-errptr n)))
        (loop for i below n do (setf (deref %cfs i) (pop cfs)))
        (rocksdb-multi-get-cf db opt cfs n keys keyns vals valns errs)))))

(defun %multi-get-cf-kv-str (db cfs keys &optional (opt (rocksdb-readoptions-create)))
  (let ((n (length keys))
        (keys (clone-strings keys nil))
        (keyns (clone-integer-list (mapcar 'length keys))))
    (with-alien ((%cfs (* (* rocksdb-column-family-handle)) (make-alien (* rocksdb-column-family-handle) n))
                 (vals (* (* (unsigned 8))) (make-alien (* (unsigned 8)) n))
                 (valns (* size-t) (make-alien size-t n))
                 (errs (* rocksdb-errptr) (make-alien rocksdb-errptr n)))
      (loop for i below n do (setf (deref %cfs i) (pop cfs)))
      (rocksdb-multi-get-cf db opt cfs n keys keyns vals valns errs))))

(defun %merge-kv (db key val &optional (opt (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :error merge-kv-error :val val)
    (rocksdb-merge db opt %key %klen %val %vlen e)))

(defun %merge-kv-str (db key val &optional (opt (rocksdb-writeoptions-create)))
  (let ((k (string-to-octets key))
        (v (string-to-octets val)))
    (%merge-kv db k v opt)))

(defun %delete-kv (db key &optional (opt (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e)
    (rocksdb-delete db opt %key %klen e)))

(defun %delete-kv-str (db key &optional (opt (rocksdb-writeoptions-create)))
  (let ((k (string-to-octets key)))
    (%delete-kv db k opt)))

;;; Column Family
(defun %open-cfs (db-opt name names opts)
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

(defun %create-cf (db name &optional (opt (rocksdb-options-create)))
  (with-errptr* (err 'cf-error :db db :cf name)
    (rocksdb-create-column-family db opt name err)))

(defun %destroy-cf (cf)
  (rocksdb-column-family-handle-destroy cf))

(defun %get-cf (db cf key &optional (opt (rocksdb-readoptions-create)) pinned)
  (with-kv-raw (db key e :error get-kv-cf-error :cf cf)
      (with-alien ((vlen (* size-t) (make-alien size-t)))
        (let ((val (if pinned
                       (rocksdb-get-pinned db opt %key %klen e)
                       (rocksdb-get-cf 
                        db
			            opt
                        cf
			            %key 
                        %klen
                        vlen
			            e)))
	      ;; helps if we know the vlen beforehand, would need a custom
	      ;; C-side function probably.
	      (v (make-array (deref vlen) :element-type 'octet)))
          (let ((ret (clone-octets-from-alien val v (deref vlen))))
            (unless (zerop (length ret))
              ret))))))

(defun %get-cf-str (db cf key &optional (opt (rocksdb-readoptions-create)) pinned)
  (let ((k (string-to-octets key :null-terminate nil)))
    (let ((v (%get-cf db cf k opt pinned)))
      (when v (octets-to-string v)))))

(defun %put-cf (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :error put-kv-cf-error :val val :cf cf)
    (rocksdb-put-cf db
                    opts
                    cf
                    %key %klen
                    %val %vlen
                    e)))

(defun %put-cf-str (db cf key val &optional (opt (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (%put-cf db cf key-octets val-octets opt)))

(defun %merge-cf (db cf key val &optional (opt (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :cf cf :error merge-kv-cf-error :val val)
    (rocksdb-merge-cf db opt cf %key %klen %val %vlen e)))

(defun %merge-cf-str (db cf key val &optional (opt (rocksdb-writeoptions-create)))
  (let ((k (string-to-octets key))
        (v (string-to-octets val)))
    (%merge-cf db cf k v opt)))

(defun %cf-name (cf-handle)
  (multiple-value-bind (name len) (rocksdb-column-family-handle-get-name cf-handle)
    (copy-c-string name (make-string len))))

(defun %cf-id (cf-handle)
  (rocksdb-column-family-handle-get-id cf-handle))

;;; Iterators
(defun %create-iter (db &optional (opt (rocksdb-readoptions-create)))
  (rocksdb-create-iterator db opt))

(defun %create-cf-iter (db cf &optional (opt (rocksdb-readoptions-create)))
  (rocksdb-create-iterator-cf db opt cf))

(defun %transaction-wbwi (self)
  (rocksdb-transaction-get-writebach-wi self))

(defun %transaction-create-iter (txn &optional (opts (rocksdb-readoptions-create)))
  (rocksdb-transaction-create-iterator txn opts))

(defun %transaction-create-iter-cf (txn cf &optional (opts (rocksdb-readoptions-create)))
  (rocksdb-transaction-create-iterator-cf txn opts cf))

(defun %transactiondb-create-iter (txndb &optional (opts (rocksdb-readoptions-create)))
  (rocksdb-transactiondb-create-iterator txndb opts))

(defun %transactiondb-create-iter-cf (txndb cf &optional (opts (rocksdb-readoptions-create)))
  (rocksdb-transactiondb-create-iterator-cf txndb opts cf))

(defun %create-iterators (db opts columns)
  (with-alien ((iters (* (* rocksdb-iterator))
                      (make-alien (* rocksdb-iterator) (length columns)))
               (cfs (* (* rocksdb-column-family-handle)) 
                    (make-alien (* rocksdb-column-family-handle) (length columns))))
    (with-errptr e
      (rocksdb-create-iterators db opts cfs iters e))))

(defun %reset-iter (iter)
  (with-errptr e (rocksdb-iter-refresh iter e)))

(defun %destroy-iter (iter)
  (rocksdb-iter-destroy iter))

(defun %iter-key (iter)
  (multiple-value-bind (key klen) (rocksdb-iter-key iter)
    (let ((k (make-array klen :element-type '(unsigned-byte 8))))
      (clone-octets-from-alien key k klen)
      k)))

(defun %iter-key-str (iter)
  (when-let ((k (%iter-key iter)))
    (octets-to-string k)))

(defun %iter-val (iter)
  (multiple-value-bind (val vlen) (rocksdb-iter-value iter)
    (let ((v (make-array vlen :element-type '(unsigned-byte 8))))
      (clone-octets-from-alien val v vlen)
      v)))

(defun %iter-valid-p (iter)
  (rocksdb-iter-valid iter))

(defun %iter-val-str (iter)
  (when-let ((v (%iter-val iter)))
    (octets-to-string v)))

;;; Backup DB
(defun %open-backup-engine (be-path &optional (opts (rocksdb-backup-engine-options-create)))
  (with-errptr* (err 'open-db-error :db be-path)
    (let ((be-path (if (pathnamep be-path)
                       (namestring be-path)
                       be-path)))
      (rocksdb-backup-engine-options-set-backup-dir opts be-path)
      (rocksdb-backup-engine-open opts be-path err))))

(defun %close-backup-engine (be)
  (rocksdb-backup-engine-close be))

(defun %create-new-backup (be db)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-backup-engine-create-new-backup be db err)))

(defun %restore-from-latest-backup (be db-path backup-path &optional (opt (rocksdb-restore-options-create)))
  (with-errptr* (err 'open-db-error)
    (rocksdb-backup-engine-restore-db-from-latest-backup be db-path backup-path opt err)))

(defun %restore-from-backup (be db-path backup-path backup-id &optional (opt (rocksdb-restore-options-create)))
  (with-errptr* (err 'open-db-error)
    (rocksdb-backup-engine-restore-db-from-backup be db-path backup-path opt backup-id err)))

(defun %backup-info (be)
  (rocksdb-backup-engine-get-backup-info be))

;;; Snapshot
(defun %create-snapshot (db)
  (rocksdb-create-snapshot db))

(defun %release-snapshot (db snapshot)
  (rocksdb-release-snapshot db snapshot))

;;; Env
(defun %destroy-env (env) (rocksdb-env-destroy env))
(defun %rocksdb-env (&optional mem)
  (if mem (rocksdb-create-mem-env) (rocksdb-create-default-env)))

(defun %rocksdb-env-get (key &optional (env (%rocksdb-env)))
  (ecase (keywordicate key)
    (:high-priority-background-threads (rocksdb::rocksdb-env-get-high-priority-background-threads env))
    (:low-priority-background-threads (rocksdb::rocksdb-env-get-low-priority-background-threads env))
    (:bottom-priority-background-threads (rocksdb::rocksdb-env-get-bottom-priority-background-threads env))
    (:background-threads (rocksdb-env-get-background-threads env))))

(defun %rocksdb-env-set (key val &optional (env (%rocksdb-env)))
  (ecase (keywordicate key)
    (:high-priority-background-threads (rocksdb::rocksdb-env-set-high-priority-background-threads env val))
    (:low-priority-background-threads (rocksdb::rocksdb-env-set-low-priority-background-threads env val))
    (:bottom-priority-background-threads (rocksdb::rocksdb-env-set-bottom-priority-background-threads env val))
    (:background-threads (rocksdb-env-set-background-threads env val))))

;;; SST
(defun %create-sst-writer (&optional (env-opts (rocksdb-envoptions-create)) (io-opts (rocksdb-options-create)))
  (rocksdb-sstfilewriter-create env-opts io-opts))

(defun %create-sst-writer-with-comparator (comparator
                                              &optional
                                                (env-opts (rocksdb-envoptions-create))
                                                (io-opts (rocksdb-options-create)))
  (rocksdb-sstfilewriter-create-with-comparator env-opts io-opts comparator))

(defun %sst-filewriter (&optional comparator
                                  (env (rocksdb-envoptions-create))
                                  (opts (rocksdb-options-create)))
  (if comparator
      (%create-sst-writer-with-comparator comparator env opts)
      (%create-sst-writer env opts)))

(defun %finish-sst-writer (writer)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-finish writer err)))

(defun %destroy-sst-writer (writer)
  (rocksdb-sstfilewriter-destroy writer))

(defun %open-sst-writer (writer name)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-open writer name err)))

;; this function is deprecated in the Java API:
;; https://javadoc.io/doc/org.rocksdb/rocksdbjni/6.6.4/org/rocksdb/SstFileWriter.html
;; (defun %sst-add (writer key val)
;;   (with-errptr* (err 'rdb-alien-error)
;;     (rocksdb-sstfilewriter-add writer key (length key) val (length val) err)))

(defun %sst-put (writer key val)
  (let ((klen (length key))
        (vlen (length val)))
    (with-errptr* (err 'rdb-alien-error)
      (with-alien ((k (* unsigned-char) (make-alien unsigned-char klen))
                   (v (* unsigned-char) (make-alien unsigned-char vlen)))
        (setfa k key)
        (setfa v val)
        (rocksdb-sstfilewriter-put writer k klen v vlen err)))))

(defun %sst-put-str (writer key val)
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (%sst-put writer key-octets val-octets)))

(defun %sst-put-ts (writer key val ts)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-put-with-ts writer key (length key) val (length val) ts (length ts) err)))

(defun %sst-delete (writer key)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-delete writer key (length key) err)))

(defun %sst-delete-ts (writer key ts)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-delete-with-ts writer key (length key) ts (length ts) err)))

(defun %sst-delete-range (writer start-key end-key)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-delete-range writer start-key (length start-key) end-key (length end-key) err)))

(defun %sst-file-size (writer)
  (with-errptr* (err 'rdb-alien-error)
    (with-alien ((ret unsigned-long))
      (rocksdb::rocksdb-sstfilewriter-file-size writer (addr ret) err)
      ret)))

;;; Transactions
(defun %open-transactiondb (opts topts name)
  (with-errptr* (e 'open-db-error :db name)
    (rocksdb-transactiondb-open opts topts name e)))

(defun %open-optimistictransactiondb (opts name)
  (with-errptr* (e 'open-db-error :db name)
    (rocksdb-optimistictransactiondb-open opts name e)))

(defun %transactiondb-get-kv (db key &optional (opts (rocksdb-readoptions-create)) pinned)
  (with-kv-raw (db key e :error get-kv-error)
    (with-alien ((vlen size-t))
      (let* ((val (if pinned
                      (rocksdb-transactiondb-get-pinned db opts %key %klen e)
                      (rocksdb-transactiondb-get db opts %key %klen vlen e)))
             (v (make-array vlen :element-type 'octet)))
        (clone-octets-from-alien val v vlen)
        v))))

(defun %transactiondb-get-kv-str (db key &optional (opts (rocksdb-readoptions-create)) pinned)
  (let ((k (string-to-octets key)))
    (let ((v (%transactiondb-get-kv db k opts pinned)))
      (when v (octets-to-string v)))))

(defun %transactiondb-get-cf (db cf key &optional (opts (rocksdb-readoptions-create)) pinned)
  (with-kv-raw (db key e :error get-kv-cf-error :cf cf)
    (with-alien ((vlen size-t))
      (let* ((val (if pinned
                      (rocksdb-transactiondb-get-pinned-cf db opts cf %key %klen e)
                      (rocksdb-transactiondb-get-cf db opts cf %key %klen vlen e)))
             (v (make-array vlen :element-type 'octet)))
        (clone-octets-from-alien val v vlen)
        v))))

(defun %transactiondb-get-cf-str (db cf key &optional (opts (rocksdb-readoptions-create)) pinned)
  (let ((k (string-to-octets key)))
    (let ((v (%transactiondb-get-cf db cf k opts pinned)))
      (when v (octets-to-string v)))))

(defun %transactiondb-put-kv (db key val &optional (opts (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :error put-kv-error :val val)
    (rocksdb-transactiondb-put db opts %key %klen %val %vlen e)))

(defun %transactiondb-put-kv-str (db key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (%transactiondb-put-kv db key-octets val-octets opts)))

(defun %transactiondb-put-cf (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :error put-kv-cf-error :val val :cf cf)
    (rocksdb-transactiondb-put-cf db
                                  opts
                                  cf
                                  %key %klen
                                  %val %vlen
                                  e)))

(defun %transactiondb-put-cf-str (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (%transactiondb-put-cf db cf key-octets val-octets opts)))

(defun %commit-transaction (txn)
  (with-errptr* (e 'rdb-alien-error)
    (rocksdb-transaction-commit txn e)))

(defun %set-savepoint (txn)
  (rocksdb-transaction-set-savepoint txn))

(defun %rollback-transaction (txn &optional savepoint)
  "Rollback a raw transaction TXN when SAVEPOINT is non-nil only rollback to last
savepoint created with ROCKSDB-TRANSACTION-SET-SAVEPOINT."
  (with-errptr* (e 'rdb-alien-error)
    (if savepoint
        (rocksdb-transaction-rollback-to-savepoint txn e)
        (rocksdb-transaction-rollback txn e))))

(defun %prepare-transaction (txn)
  (with-errptr* (e 'rdb-transaction-error :txn txn)
    (rocksdb-transaction-prepare txn e)))

(defun %transaction-name (txn)
  (multiple-value-bind (name len) (rocksdb-transaction-get-name txn)
    (copy-c-string name (make-string len))))

(defun %set-transaction-name (txn name)
  (with-errptr* (e 'rdb-transaction-error :txn txn)
    (let ((nlen (length name)))
      (with-alien ((%name (* unsigned-char) (octets-to-alien (string-to-octets name))))
        (rocksdb-transaction-set-name txn %name nlen e)))))

(defsetf %transaction-name %set-transaction-name)

(defun %transaction-iterator (self &key column (opts (rocksdb-readoptions-create)))
  (if column
      (%transaction-create-iter-cf self column opts)
      (%transaction-create-iter self opts)))

(defun %abort-transaction (self &optional savepoint)
  (%rollback-transaction self savepoint)
  (rocksdb-transaction-destroy self))

(defun %transaction-get (txn key &optional (opts (rocksdb-readoptions-create)) pinned)
  (with-txn-raw (txn e :key key)
    (if pinned
        (rocksdb-transaction-get-pinned txn opts %key %klen e)
        (rocksdb-transaction-get txn opts %key %klen e))))

(defun %transaction-get-cf (txn cf key &optional (opts (rocksdb-readoptions-create)) pinned)
  (with-txn-raw (txn e :key key :cf cf)
    (if pinned
        (rocksdb-transaction-get-pinned-cf txn opts cf %key %klen e)
        (rocksdb-transaction-get-cf txn opts cf %key %klen e))))

(defun %transaction-delete (txn key)
  (with-txn-raw (txn e :key key)
    (rocksdb-transaction-delete txn %key %klen e)))

(defun %transaction-delete-cf (txn cf key)
  (with-txn-raw (txn e :key key :cf cf)
    (rocksdb-transaction-delete-cf txn cf %key %klen e)))

(defun %transaction-put (txn key val)
  (with-txn-raw (txn e :key key :val val)
    (rocksdb-transaction-put txn %key %klen %val %vlen e)))

(defun %transaction-put-cf (txn cf key val)
  (with-txn-raw (txn e :cf cf :key key :val val)
    (rocksdb-transaction-put-cf txn cf %key %klen %val %vlen e)))

(defun %transaction-merge (txn key val)
  (with-txn-raw (txn e :key key :val val)
    (rocksdb-transaction-merge txn %key %klen %val %vlen e)))

(defun %transaction-merge-cf (txn cf key val)
  (with-txn-raw (txn e :key key :val val :cf cf)
    (rocksdb-transaction-merge-cf txn cf %key %klen %val %vlen e)))

(defun %get-prepared-transactions (txn-db)
  "Return an array of prepared ROCKSDB-TRANSACTION pointers from this
transaction-db."
  (with-errptr* (e 'rdb-alien-error :db txn-db)
    (rocksdb-transactiondb-get-prepared-transactions txn-db)))

;;; Checkpoints
(defun %make-checkpoint (db)
  (with-errptr* (e 'rdb-alien-error :db db)
    (rocksdb-checkpoint-object-create db e)))

(defun %create-checkpoint (chk dir &optional log-size-for-flush)
  (with-errptr* (e 'rdb-alien-error)
    (rocksdb-checkpoint-create chk dir log-size-for-flush e)))

;;; Secondary
(defun %open-db-secondary (opts name sname)
  (with-errptr* (e 'open-db-error)
    (rocksdb-open-as-secondary opts name sname e)))

(defun %open-cfs-secondary (opts name sname cf-names cf-opts)
  (with-errptr* (e 'rdb-alien-error)
    (with-alien ((cf-handles (* (* rocksdb-column-family-handle))))
      (rocksdb-open-as-secondary-column-families 
       opts name sname (length cf-names) cf-names cf-opts cf-handles e))))

;;; Read-only
(defun %open-cfs-read-only (opts name cf-names cf-opts &optional err-if-wal)
  (with-errptr* (e 'rdb-alien-error)
    (with-alien ((cf-handles (* (* rocksdb-column-family-handle))))
      (rocksdb-open-for-read-only-column-families 
       opts name (length cf-names) cf-names cf-opts cf-handles err-if-wal e))))

;;; TTL
(defun %open-cfs-with-ttl (opts name cf-names cf-opts ttls)
    (with-errptr* (e 'rdb-alien-error)
      (with-alien ((cf-handles (* (* rocksdb-column-family-handle))))
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
   ;; static address to alien-callable
   (alien-sap (alien-callable-function 'rocksdb-log-default)) 
   nil))

;;; Writebatch/WBWI
(defun %create-wbwi (&optional (reserved-bytes 0) (overwrite-keys 1))
  (rocksdb-writebatch-wi-create reserved-bytes overwrite-keys))
(defun %wbwi-count (self) (rocksdb-writebatch-wi-count self))
(defun %wbwi-data (wbwi)
  (multiple-value-bind (data size) (rocksdb-writebatch-wi-data wbwi)
    (clone-octets-from-alien data (make-array size :element-type 'octet))))
(defun %writebatch-data (wb)
  (multiple-value-bind (data size) (rocksdb-writebatch-data wb)
    (clone-octets-from-alien data (make-array size :element-type 'octet))))
(defun %wbwi-clear (wbwi)
  (rocksdb-writebatch-wi-clear wbwi))
(defun %wbwi-save (self)
  (rocksdb-writebatch-wi-set-save-point self))
(defun %wbwi-ts (self ts)
  (with-errptr e
    (rocksdb-writebatch-wi-update-timestamps 
     self (octets-to-alien ts) (length ts) nil nil e)))
(defun %writebatch-iter (self)
  (rocksdb-writebatch-iterate self nil nil (alien-callable-function 'rocksdb-delete-value)))
(defun %wbwi-iter (wbwi &key state
                             put
                             (deleted (sb-alien:alien-callable-function 'rocksdb-delete-value)))
  (rocksdb-writebatch-wi-iterate wbwi state put deleted))

(defun %destroy-wbwi (self)
  (rocksdb-writebatch-wi-destroy self))
(defun %wbwi-put-cf (wbwi cf key val)
  (with-kv-raw* key val
    (rocksdb-writebatch-wi-put-cf 
     wbwi
     cf
     %key %klen
     %val %vlen)))
(defun %wbwi-write (db batch &optional (opts (rocksdb-readoptions-create)))
  (with-errptr e (rocksdb-write-writebatch-wi db opts batch e)))
(defun %wbwi-put-kv (self key val)
  (declare (octet-vector key val))
  (rocksdb-writebatch-wi-put 
   self
   (cast (octets-to-alien key) (array unsigned-char))
   (length key) 
   (cast (octets-to-alien val) (array unsigned-char))
   (length val)))

(defun %wbwi-put-kv-str (self key val)
  (%wbwi-put-kv self (string-to-octets key) (string-to-octets val)))

(defun %wbwi-kv (self key &optional (opt (rocksdb-readoptions-create)))
  (with-errptr e
    (multiple-value-bind (data i)
        (rocksdb-writebatch-wi-get-from-batch
         self
         opt
         (cast (octets-to-alien key) (array unsigned-char))
         (length key)
         e)
      (std:clone-octets-from-alien 
       data
       (make-array i :element-type 'octet)))))

(defun %wbwi-kv-str (self key &optional (opt (rocksdb-readoptions-create)))
  (let ((k (string-to-octets key)))
    (let ((v (%wbwi-kv self k opt)))
      (when v (octets-to-string v)))))

;;; zero-copy
(defun %get-kv-pinned (db key &optional (opt (rocksdb-readoptions-create)))
  "DB get using the v2 zero-copy API."
  (with-kv-raw (db key e :error get-kv-error)
    (rocksdb-get-pinned-v2 db opt %key %klen e)))

(defun %get-kv-cf-pinned (db key cf &optional (opt (rocksdb-readoptions-create)))
  "DB get CF using the v2 zero-copy API."
  (with-kv-raw (db key e :error get-kv-cf-error :cf cf)
    (rocksdb-get-pinned-cf-v2 db opt cf %key %klen e)))

(defun %get-kv-buffer (db key buffer &optional (opt (rocksdb-readoptions-create)))
  "DB get using the 'into_buffer' API."
  (with-kv-raw (db key e :error get-kv-error)
    (rocksdb-get-into-buffer db opt %key %klen buffer (length buffer) e)))

(defun %get-kv-cf-buffer (db key cf buffer &optional (opt (rocksdb-readoptions-create)))
  "DB get CF using the 'into_buffer' API."
  (with-kv-raw (db key e :error get-kv-cf-error :cf cf)
    (rocksdb-get-into-buffer-cf db opt cf %key %klen buffer (length buffer) e)))
