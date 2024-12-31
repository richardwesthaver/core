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
  ((path)
   (logger)
   (schema)))

(defmethod print-object ((self rdb-config) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S ~A" :id (format-sxhash (id:id self)))))

(defun find-rdb-symbol (s)
  (find-symbol* (symbol-name s) :rdb nil))

(defmethod load-ast ((self rdb-config))
  (with-slots (ast) self
    (if (formp ast)
        ;; ast is valid, modify object, set ast nil
        (progn
          (sb-int:doplist (k v) ast
            (when-let ((s (find-rdb-symbol k))) ;; needs to be correct package
              (unless (null v)
                (setf v
                      (case k
                        (:logger (make-config :logger :ast v))
                        (t v)))
                (setf (slot-value self s) v))))
          (setf (ast:ast self) nil)
          self)
        ;; invalid ast, signal error
        (error 'syntax-error))))
  
(defmethod build-ast ((self rdb-config) &key (nullp nil) (exclude '(ast id)))
  (setf (ast self)
        (unwrap-object self
                       :slots t
                       :methods nil
                       :nullp nullp
                       :exclude exclude)))

(defmethod build ((self rdb-config) &key)
  (make-db (slot-value self 'backend) 
           :opts (slot-value self 'options) 
           :logger (when-let ((l (slot-value self 'logger))) (build l))
           :name (slot-value self 'path)))

(defmethod make-config ((self (eql :rdb)) &rest args)
  (apply 'make-instance 'rdb-config args))
