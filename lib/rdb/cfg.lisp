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

;;; Options
;; TODO 2026-08-02: this whole rdb-opts thing needs work - eliminate struct
;; wrappers and use our 'options' api.
;;; Options
;; These expand into lookup macros for the pre-defined option GET and SET
;; functions - for example RDB-OPT-SETTER and RDB-OPT-GETTER.
(eval-always
  (macrolet ((%def-opt-finders (name)
               `(progn 
                  (defmacro ,(symbolicate name '-setter) (key)
                    `(or (find-symbol (format nil "~:@(~A-SET-~A~)" ',',name ,key) :rocksdb)
                         (when (string= (string-downcase ,key) "event-listener")
                           'rocksdb:rocksdb-options-add-eventlistener)))
                  (defmacro ,(symbolicate name '-getter) (key)
                    `(find-symbol (format nil "~:@(~A-GET-~A~)" ',',name ,key) :rocksdb)))))
    (%def-opt-finders rocksdb-options)
    (%def-opt-finders rocksdb-writeoptions)
    (%def-opt-finders rocksdb-flushoptions)
    (%def-opt-finders rocksdb-transactiondb-options)
    (%def-opt-finders rocksdb-readoptions)
    (%def-opt-finders rocksdb-compactoptions)
    (%def-opt-finders rocksdb-ingestexternalfileoptions)
    (%def-opt-finders rocksdb-backup-engine-options)
    (%def-opt-finders rocksdb-transaction-options)
    (%def-opt-finders rocksdb-optimistictransaction-options)
    (%def-opt-finders rocksdb-lru-cache-options))
  (macrolet ((%defopt (name)
               (let ((%create (symbolicate name '-create)))
                 `(defun ,(symbolicate 'make- name) (&optional init-fn)
                    ,(format nil "Make and return a ~A alien object.
INIT-FN is an optional argument which must be a lambda which takes a single
parameter (the object itself). It is used to initialize the instance with
custom configuration." name)
                    (let ((opts (,%create)))
                      (when init-fn (funcall init-fn opts))
                      opts)))))
    (%defopt rocksdb-options)
    (%defopt rocksdb-readoptions)
    (%defopt rocksdb-writeoptions)
    (%defopt rocksdb-transaction-options)
    (%defopt rocksdb-transactiondb-options)
    (%defopt rocksdb-lru-cache-options)
    (%defopt rocksdb-backup-engine-options))
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
      (%mktbl 'rocksdb-compactoptions *rocksdb-compactoptions*))
    (defparameter *rdb-flushopts-table*
      (%mktbl 'rocksdb-flushoptions *rocksdb-flushoptions*))
    (defparameter *rdb-lru-cache-opts-table*
      (%mktbl 'rocksdb-lru-cache-options *rocksdb-lru-cache-options*)))
    ;; (defparameter *trdb-opts-table*
    ;;   (%mktbl 'rocksdb-transactiondb-options *rocksdb-transactiondb-options*))
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
    (%def-opt rocksdb-options "parallelism" "enable-statistics" "event-listener")
    (%def-opt rocksdb-readoptions)
    (%def-opt rocksdb-writeoptions)
    (%def-opt rocksdb-flushoptions)
    (%def-opt rocksdb-backup-engine-options)
    (%def-opt rocksdb-compactoptions)
    (%def-opt rocksdb-transaction-options)
    (%def-opt rocksdb-transactiondb-options)
    (%def-opt rocksdb-optimistictransaction-options)
    (%def-opt rocksdb-lru-cache-options)
    (%def-opt rocksdb-ingestexternalfileoptions))
  (macrolet ((define-rdb-opt-struct (name &rest defaults)
               (let ((%creator (symbolicate name '-create))
                     (%default (symbolicate 'default- name)))
               `(prog1
                    (eval-always
                      (defun ,name (&rest opts)
                        (let ((obj (,%creator)))
                          (loop for (k v) on opts by #'cddr while v
                                do (let ((k (typecase k
                                              (string (string-downcase k))
                                              (symbol (string-downcase (symbol-name k)))
                                              (t (string-downcase (format nil "~s" k))))))
                                     (,(symbolicate '%set- name) obj k v)))
                          obj)))
                  (defun ,%default ()
                    (,name ,@defaults))
                  (defvar ,(symbolicate '*default- name '*) (,%default))))))
    (define-rdb-opt-struct rocksdb-options
      :create-if-missing t 
      :create-missing-column-families t 
      :parallelism (num-cpus)
      :compression (rocksdb-compression-type :zstd))
    (define-rdb-opt-struct rocksdb-readoptions)
    (define-rdb-opt-struct rocksdb-writeoptions)
    (define-rdb-opt-struct rocksdb-compactoptions)
    (define-rdb-opt-struct rocksdb-backup-engine-options)
    (define-rdb-opt-struct rocksdb-flushoptions)
    (define-rdb-opt-struct rocksdb-lru-cache-options)
    (define-rdb-opt-struct rocksdb-transaction-options)
    (define-rdb-opt-struct rocksdb-transactiondb-options)
    (define-rdb-opt-struct rocksdb-optimistictransaction-options)
    (define-rdb-opt-struct rocksdb-ingestexternalfileoptions)))

(defconfig rdb-config (simple-db-config)
  ((logger :initform (default-logger-config) :initarg :logger :type (or null log::logger-config)))
  (:default-initargs
   :backend :rdb
   :schema (make-instance 'rdb-schema)
   :options *default-rocksdb-options*))

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
