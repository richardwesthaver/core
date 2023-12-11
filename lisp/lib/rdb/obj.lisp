(in-package :rdb)

;;; rdb-opts
(defstruct rdb-opts
  (create-if-missing nil :type boolean)
  (total-threads 1 :type integer) ;; numcpus is default
  (max-open-files 10000 :type integer)
  (use-fsync nil :type boolean)
  (disable-auto-compactions nil :type boolean)
  (sap nil :type (or null alien)))

(defun default-rdb-opts () 
  (make-rdb-opts
   :create-if-missing t 
   :total-threads 4))

;;; rdb-cf
(defstruct rdb-cf
  (name "" :type string)
  (sap nil :type (or null alien)))

(defun create-cf (db cf)
  (setf (rdb-cf-sap cf)
        (with-errptr err
          (rocksdb-create-column-family db (rocksdb-options-create) (rdb-cf-name cf) err))))

;;; rdb
(defstruct rdb
  (name "" :type string)
  (opts (default-rdb-opts) :type rdb-opts)
  (db nil :type (or null alien))
  (cfs (make-array 0 :element-type 'rdb-cf :adjustable t :fill-pointer 0) :type (array rdb-cf)))

(defmethod push-cf ((cf rdb-cf) (db rdb))
  (vector-push cf (rdb-cfs db)))

(defmethod open-rdb ((self rdb))
  (setf (rdb-db self)
        (open-db-raw (rdb-name self) (rdb-opts self))))

(defmethod close-rdb ((self rdb))  
  (close-db-raw (rdb-db self))
  (setf (rdb-db self) nil))

(defmethod destroy-rdb ((self rdb))  
  (when (rdb-db self) (close-rdb self))
  (destroy-db-raw (rdb-name self)))

(defmethod init-cfs ((self rdb))
  (loop for cf across (rdb-cfs self)
        do (create-cf (rdb-db self) cf)))

(defmethod insert-kv ((self rdb) key val &key cf)
  (if cf
    (put-cf-raw
     (rdb-db self)
     (rdb-cf-sap (find cf (rdb-cfs self) :key #'rdb-cf-name :test #'equal))
     key
     val)
    (put-kv-raw
     (rdb-db self)
     key 
     val)))

(defmethod insert-kv-str ((self rdb) key val &key cf)
  (insert-kv self (string-to-octets key) (string-to-octets val) :cf cf))
