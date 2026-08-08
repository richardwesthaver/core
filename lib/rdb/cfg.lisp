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
;; These expand into lookup macros for the pre-defined option GET and SET
;; functions.
(macrolet ((%defopt (name &optional set-only default)
             (let ((%creator (symbolicate name '-create))
                   (%default (symbolicate 'default- name))
                   (%opt (symbolicate (string-right-trim "S" (string name)))))
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
                    (let ((obj (,%creator)))
                      (loop for (k v) on opts by #'cddr while v
                            do (let ((k (typecase k
                                          (string (string-downcase k))
                                          (symbol (string-downcase (symbol-name k)))
                                          (t (string-downcase (format nil "~s" k))))))
                                 (setf (,%opt k obj) v)))
                      obj))
                  (defun ,%default ()
                    (,name ,@default))
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
                  (defvar ,(symbolicate '*default- name '*) (,%default))))))
  (%defopt rocksdb-options 
           ("parallelism" "enable-statistics" "event-listener")
           (:create-if-missing t 
            :create-missing-column-families t 
            :parallelism (num-cpus)
            :compression (rocksdb-compression-type :zstd)
            :enable-pipelined-write t))
  (%defopt rocksdb-readoptions)
  (%defopt rocksdb-writeoptions)
  (%defopt rocksdb-transaction-options)
  (%defopt rocksdb-transactiondb-options)
  (%defopt rocksdb-lru-cache-options)
  (%defopt rocksdb-backup-engine-options))

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
