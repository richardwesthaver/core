;;; cfg.lisp --- RDB Configuration

;; Base Configuration Protocol for RDB Databases and Stores

;;; Commentary:

;; The RDB-CONFIG object may be used to specify initialization values for
;; RDB-DATABASE/RDB/RDB-STORE. You may call MAKE-DB on RDB-CONFIG to return a
;; RocksDB handle or object instance.

;; Support for the various RocksDB option types is exposed via the OPTIONS
;; protocol.

;;; Code:
(in-package :rdb)

;;; Options
;; TODO 2026-08-02: this whole rdb-opts thing needs work - eliminate struct
;; wrappers and use our 'options' api.
;;; Options
(macrolet ((%defopt (name fn)
             `(defun ,(symbolicate 'make- name) (&optional init-fn)
                ,(format nil "Make and return a ~A alien object.
INIT-FN is an optional argument which must be a lambda which takes a single
parameter (the object itself). It is used to initialize the instance with
custom configuration." name)
                (let ((opts (,fn)))
                  (when init-fn (funcall init-fn opts))
                  opts))))
  (%defopt rocksdb-options rocksdb-options-create)
  (%defopt rocksdb-readoptions rocksdb-readoptions-create)
  (%defopt rocksdb-writeoptions rocksdb-writeoptions-create)
  (%defopt rocksdb-transaction-options rocksdb-transaction-options-create)
  (%defopt rocksdb-transactiondb-options rocksdb-transactiondb-options-create)
  (%defopt rocksdb-backup-engine-options rocksdb-backup-engine-options-create))

(defun default-rocksdb-options ()
  (make-rocksdb-options
   (lambda (o) (rocksdb-options-set-create-if-missing o t))))

(flet ((%mktbl (accessor opts)
         (let ((table (make-hash-table :test #'equal)))
           (mapc (lambda (x) (setf (gethash (car x) table) (cdr x)))
                 (loop for y across opts
                       collect (cons y (intern (format nil "~:@(~A-set-~x~)" accessor y) :rocksdb))))
           table)))
  (defparameter *rdb-opts-table*
    (let ((tbl (%mktbl 'rocksdb-options *rocksdb-options*)))
      (setf (gethash "event-listener" tbl) 'rocksdb:rocksdb-options-add-eventlistener)
      tbl))
  (defparameter *rdb-readopts-table*
    (%mktbl 'rocksdb-readoptions *rocksdb-readoptions*))
  (defparameter *rdb-writeopts-table*
    (%mktbl 'rocksdb-writeoptions *rocksdb-writeoptions*))
  (defparameter *rdb-backupopts-table*
    (%mktbl 'rocksdb-backup-engine-options *rocksdb-backup-engine-options*))
  (defparameter *rdb-ingestopts-table*
    (%mktbl 'rocksdb-ingestexternalfileoptions *rocksdb-ingestexternalfileoptions*))
  (defparameter *rdb-compactopts-table*
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

(defconfig rdb-config (simple-db-config)
  ((logger :initform (default-logger-config) :initarg :logger :type (or null log::logger-config)))
  (:default-initargs
   :backend :rdb
   :schema (make-instance 'rdb-schema)
   :options *default-rdb-opts*))

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

(defmethod build ((self rdb-config) &key (nullp nil) (exclude '(ast id schema logger options)))
  (setf (ast self)
        (unwrap-object self
                       :slots t
                       :methods nil
                       :nullp nullp
                       :exclude exclude))
  (when (slot-boundp self 'schema) 
    (appendf (ast self) (list :schema (ast (slot-value self 'schema)))))
  (when (slot-boundp self 'logger) 
    (appendf (ast self) (list :logger (ast (slot-value self 'logger)))))
  (when (slot-boundp self 'options)
    (appendf (ast self) (list :options (ast (slot-value self 'options)))))
  self)

(defun build-rdb-config (self)
  (make-db (slot-value self 'backend)
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
