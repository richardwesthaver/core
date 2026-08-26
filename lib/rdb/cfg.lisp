;;; cfg.lisp --- RDB Configuration

;; Base Configuration Protocol for RDB Databases and Stores

;;; Commentary:

;; The RDB-CONFIG object may be used to specify initialization values for
;; RDB/RDB-STORE. You may call MAKE-DB on RDB-CONFIG to return a
;; RocksDB handle or object instance.

;; Support for the various RocksDB option types is exposed via the OPTIONS
;; protocol.

;;; Code:
(in-package :rdb)

;; temp-rdb
(defvar *temp-db-path-generator*
  (lambda (&optional (name "temp-db"))
    (make-pathname :directory "tmp" :name (symbol-name (gensym name))))
  "A single arg function returning the absolute path to a temp-db path.")

(defvar *temp-db-destroy* nil)

;;; Options
#+nil (defvar *default-opt-table* (make-hash-table))

(macrolet ((%defopt (name &optional set-only &rest default)
             (let ((%creator (symbolicate name '-create))
                   (%default (symbolicate 'default- name))
                   (%opt (symbolicate (string-right-trim "S" (string name)))))
               (with-gensyms (%obj)
                 `(progn
                    (defun ,(symbolicate 'make- name) (&optional init-fn)
                      ,(format nil "Make and return a ~A alien object.
INIT-FN is an optional argument which must be a lambda which takes a single
parameter (the object itself). It is used to initialize the instance with
custom configuration." name)
                      (let ((opts (,%creator)))
                        (when init-fn (funcall init-fn opts))
                        opts))
                    (defun ,(symbolicate '% name '-no-getter-p) (key)
                      (let ((k (typecase key
                                 (string (string-downcase key))
                                 (symbol (string-downcase (symbol-name key)))
                                 (t (string-downcase (format nil "~s" key))))))
                        (memq t (mapcar
                                 (lambda (x) (equal k x))
                                 ',set-only))))
                    (defun ,name (&rest opts)
                      (let ((,%obj (,%creator)))
                        (loop for (k v) on opts by #'cddr while v
                              do (let ((k (typecase k
                                            (string (string-downcase k))
                                            (symbol (string-downcase (symbol-name k)))
                                            (t (string-downcase (format nil "~s" k))))))
                                   (setf (,%opt k ,%obj) v)))
                        ,%obj))
                    (defun ,%default ()
                      (,name ,@default))
                    (defun ,(symbolicate %default '*) (&rest opts)
                      (let ((,%obj (,%default)))
                        (loop for (k v) on opts by #'cddr while v
                              do (let ((k (typecase k
                                            (string (string-downcase k))
                                            (symbol (string-downcase (symbol-name k)))
                                            (t (string-downcase (format nil "~s" k))))))
                                   (setf (,%opt k ,%obj) v)))
                        ,%obj))
                    (defmacro ,(symbolicate name '-setter) (key)
                      `(or (find-symbol (format nil "~:@(~A-SET-~A~)" ',',name ,key) :rocksdb)
                           (when (string= (string-downcase ,key) "event-listener")
                             'rocksdb:rocksdb-options-add-eventlistener)))
                    (defmacro ,(symbolicate name '-getter) (key)
                      `(find-symbol (format nil "~:@(~A-GET-~A~)" ',',name ,key) :rocksdb))
                    (defun ,%opt (key &optional (opt (,%default)))
                      (if-let ((g (,(symbolicate name '-getter) key)))
                        (funcall g opt)
                        (warn 'opt-handler-missing :message key)))
                    (defun (setf ,%opt) (val key opt)
                      (funcall (,(symbolicate name '-setter) key) opt val))
                    #+nil (setf (gethash ,(keywordicate name) *default-opt-table*) #',%default))))))
  (%defopt rocksdb-readoptions
           ("snapshot" "iterate-upper-bound" "iterate-lower-bound" "prefix-same-as-start"
            "ignore-range-deletions" "timestamp" "iter-start-ts")
           :async-io t
           :prefix-same-as-start t
           :pin-data t
           :auto-readahead-size t)
  (%defopt rocksdb-writeoptions)
  (%defopt rocksdb-transaction-options
           ("set-snapshot" "deadlock-detect" "lock-timeout" "expiration" 
                           "deadlock-detect-depth" "max-write-batch-size" "skip-prepare")
           ;; :skip-prepare t
           ;; :expiration 100000
           )
  (%defopt rocksdb-optimistictransaction-options
           ("set-snapshot"))
  (%defopt rocksdb-wait-for-compact-options
           ()
           :close-db t
           :timeout 100000)
  (%defopt rocksdb-transactiondb-options
           ("max-num-locks" "num-stripes" "transaction-lock-timeout")
           :max-num-locks 2000
           :transaction-lock-timeout 100000
           ;; :write-policy :write-unprepared
           )
  (%defopt rocksdb-lru-cache-options
           ("capacity" "num-shard-bits" "memory-allocator")
           :capacity 10485760)
  (%defopt rocksdb-hyper-clock-cache-options
           ("capacity" "num-shard-bits" "memory-allocator" "estimated-entry-charge")
           :capacity 10485760
           ;; experimental: determine dynamically
           :estimated-entry-charge 0)
  (%defopt rocksdb-universal-compaction-options)
  (%defopt rocksdb-compactoptions)
  (%defopt rocksdb-fifo-compaction-options)
  (%defopt rocksdb-envoptions)
  (%defopt rocksdb-flushoptions)
  (%defopt rocksdb-backup-engine-options
           ("backup-dir" "env"))
  (%defopt rocksdb-ingestexternalfileoptions
           ("move-files" "snapshot-consistency" "allow-global-seqno" "allow-blocking-flush" "ingest-behind"
            "fail-if-not-bottommost-level"))
  (%defopt rocksdb-block-based-options
           ("separate-key-value-in-data-block" "index-type" "data-block-index-type" "data-block-hash-ratio"
            "cache-index-and-filter-blocks" "cache-index-and-filter-blocks-with-high-priority"
            "pin-l0-filter-and-index-blocks-in-cache" "pin-top-level-index-and-filter" 
            "top-level-index-pinning-tier" "partition-pinning-tier" "unpartition-pinning-tier"
            "uniform-cv-threshold" "fifo-compaction-options")
           :filter-policy (rocksdb-filterpolicy-create-ribbon 10.d0))
  (%defopt rocksdb-options 
           ("parallelism" "enable-statistics" "event-listener" "block-based-table-factory" "compression-options"
            "merge-operator" "db-log-dir" "wal-dir" "wal-ttl-seconds" "wal-size-limit-mb" "memtable-vector-rep"
            "prepare-for-bulk-load" "universal-compaction-options" "hash-skip-list-rep" "plain-table-factory"
            "min-level-to-compress" "ratelimiter" "row-cache" "prefix-extractor" "compaction-service" "env")
           :create-if-missing t
           :create-missing-column-families t
           :parallelism (num-cpus)
           :compression (rocksdb-compression-type :zstd)
           :enable-pipelined-write t
           :error-if-exists nil
           :block-based-table-factory (default-rocksdb-block-based-options)
           ;; :sst-partitioner-factory (rocksdb-sst-partitioner-fixed-prefix-factory-create 8)
           ;; :prefix-extractor (create-fixed-prefix-op 8)
           :memtable-prefix-bloom-size-ratio 0.1d0))

;;; Env
(defun rocksdb-env (&rest args)
  (remf args :mem)
  (let ((env (%rocksdb-env (getf args :mem))))
    (doplist (k v) args
      (%rocksdb-env-set k v env))
    env))

(defun ensure-rocksdb-directory (path &optional (env (rocksdb-env)))
  (with-errptr e
    (rocksdb-create-dir-if-missing env (namestring path) e)
    (pathname path)))

;;; Config
(defconfig rdb-config (simple-db-config)
  ((logger :initform (default-logger-config) :initarg :logger :type (or null log::logger-config)))
  (:default-initargs
   :engine :rdb
   :schema (make-instance 'rdb-schema)
   :options (default-rocksdb-options)))

(defmethod print-object ((self rdb-config) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S ~A" :id (id:id self))))

(defmethod load-ast ((self rdb-config))
  (with-slots (ast) self
    (if (formp ast)
        ;; ast is valid, modify object, set ast nil
        (let ((new-ast))
          (sb-int:doplist (k v) ast
            (when-let ((s (find-symbol (string k)))) ;; needs to be correct package
              (case k
                (:logger (setf (slot-value self s) (make-config :logger :ast v)))
                (:schema (setf (slot-value self s) (apply 'make-schema v)))
                (:id (setf (slot-value self s) v))
                (:options (setf (ast (slot-value self s)) v))
                (t (nconsc new-ast (list k v))))))
          (setf (ast self) new-ast)
          self)
        ;; invalid ast, signal error
        (error 'syntax-error :ast ast))))

;; (defmethod build ((self rdb-config) &key (nullp nil) (exclude '(ast id schema logger options)))
;;   (setf (ast self)
;;         (unwrap-object self
;;                        :slots t
;;                        :methods nil
;;                        :nullp nullp
;;                        :exclude exclude))
;;   (when (slot-boundp self 'schema) 
;;     (appendf (ast self) (list :schema (ast (slot-value self 'schema)))))
;;   (when (slot-boundp self 'logger) 
;;     (appendf (ast self) (list :logger (ast (slot-value self 'logger)))))
;;   (when (slot-boundp self 'options)
;;     (appendf (ast self) (list :options (ast (slot-value self 'options)))))
;;   self)

(defmethod build ((self rdb-config) &key)
  (make-db (slot-value self 'engine)
    :opts (slot-value self 'options)
    :logger (when-let ((l (slot-value self 'logger))) (build l))
    :name (slot-value self 'path)))

(defmethod make-config ((self (eql :rdb)) &rest args)
  (apply 'make-instance 'rdb-config args))

(defun init-dbrc (&optional (file #p"xdg:config;dbrc"))
  (let ((cfg (make-instance 'rdb-config)))
    (build cfg)
    (with-open-file (out file
			             :direction :output
			             :if-does-not-exist :create)
      (write (ast cfg) :stream out :pretty t :case :downcase))))
