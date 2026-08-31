;;; simple.lisp --- Simple RDB

;;

;;; Code:
(in-package :rdb)

(defclass srdb (rdb)
  ((backup :initform nil :type (or null (alien (* rocksdb-backup-engine))) :initarg :backup :accessor db-backup)
   (snapshots :initform nil
              :initarg :snapshots 
              :accessor snapshots)
   (checkpoints :initform nil
                :initarg :checkpoints
                :accessor checkpoints))
  (:default-initargs 
   :columns nil))

(defmethod make-db ((engine (eql :srdb)) &rest initargs &key (load t) open)
  (declare (ignore engine))
  (remove-from-plist initargs :open :load)
  (let ((db (apply 'make-instance 'srdb initargs)))
    (when (and load (probe-file (path db))) (load-opts db))
    (when open (open-db db))
    db))

(defmethod checkpoint :around ((self srdb) &rest args)
  (when-let ((chk (apply 'call-next-method args)))
    (push chk (checkpoints self))))

(defmethod snapshot :around ((self srdb) &key)
  (push
   (call-next-method self)
   (snapshots self)))

(defmethod backup :around ((self srdb) &rest args)
  (setf (db-backup self) (apply 'call-next-method args)))

(defmethod shutdown-db :around ((self srdb) &key wait)
  (close-backup self)
  (call-next-method self :wait wait))

(defmethod multi-get ((self srdb) keys &key (data-type 'octet-vector) (opts (rocksdb-readoptions-create)) cf)
  (if cf
      (ecase data-type
        (octet-vector (%multi-get-cf-kv (sap self) keys opts (sap cf)))
        (string (%multi-get-cf-kv-str (sap self) keys opts (sap cf))))
      (ecase data-type
        (octet-vector (%multi-get-kv (sap self) keys opts))
        (string (%multi-get-kv-str (sap self) keys opts)))))

(defmethods insert-key 
  (((self srdb) (key string) (val string) &key column)
   (if-let ((column (and column (find-column column self))))
     (if-let ((sap (sap column)))
       (%put-cf
        (sap self)
        sap
        (string-to-octets key)
        (string-to-octets val)
        (rocksdb-writeoptions-create))
       (simple-rdb-error "column-family is not open"))
     (put-key self key val)))
  (((self srdb) (key string) val &key column)
   (if-let ((column (and column (find-column column self))))
     (if-let ((sap (sap column)))
       (%put-cf
        (sap self)
        sap
        (string-to-octets key)
        val
        (rocksdb-writeoptions-create))
       (simple-rdb-error "column-family is not open"))
     (put-key self key val)))
  (((self srdb) key (val string) &key column)
   (if-let ((column (and column (find-column column self))))
     (if-let ((sap (sap column)))
       (%put-cf
        (sap self)
        sap
        key
        (string-to-octets val)
        (rocksdb-writeoptions-create))
       (simple-rdb-error "column-family is not open"))
     (put-key self key val))))
