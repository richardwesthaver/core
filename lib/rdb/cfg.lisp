;;; cfg.lisp --- RDB Configuration

;; Base Configuration Protocol for RDB Databases and Stores

;;; Commentary:

;; The RDB-CONFIG object may be used to specify initialization values for
;; RDB-DATABASE/RDB/RDB-STORE.

;; You may call BUILD on an RDB-CONFIG to return the uninitialize RDB db or
;; store.

;;; Code:
(in-package :rdb)

(defconfig rdb-config (ast id db-config)
  ((path :initarg :path :type (or pathname string))
   (logger :initform (default-logger-config) :initarg :logger :type (or null log::logger-config))
   (schema :initform (make-instance 'rdb-schema) :initarg :schema :type rdb-schema))
  (:default-initargs 
   :backend :rdb
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
    (appendf (ast self) (list :logger (ast (build (slot-value self 'logger))))))
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

(defun init-rdbrc (&optional (file (merge-homedir-pathnames ".rdbrc")))
  (let ((cfg (make-instance 'rdb-config)))
    (build-rdb-config cfg)
    (with-open-file (out file
			 :direction :output
			 :if-does-not-exist :create)
      (write-ast cfg out :pretty t :case :downcase))))
