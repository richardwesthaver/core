;;; mpk/db.lisp --- MPK Database

;; Internal MPK Databases

;;; Commentary:

;; We initially started MPK with a single database attached - MDB (Media Database)

;; Since then our capabilities have grown and we need to manage many different
;; media types concurrently - A single RDB instance is too limiting for this.

;;; Code:
(in-package :mpk/db)
(in-readtable :std)
(defvar *mpk-db-backend-options* rdb::*rdb-backend-options*)
(set-database-backend 
 :mpk *mpk-db-backend-options*
 (lambda () (db::%load-database-backend :rdb)))

(defschema mpk-db-schema (rdb-schema)
  ((:id (uuid . string))
   (:file (uuid . string))
   (:name (uuid . string))
   (:source (uuid . string))
   (:state (uuid . octet))
   (:meta (uuid . string))))

(defvar *mpk-db-schema* (make-instance 'mpk-db-schema))

(load-database-backend :mpk)

;;; Config
(defconfig mpk-db-config (db-config) ()
  (:default-initargs :backend :mpk))

;;; ID
(defclass mpk-db-id (id) ()
  (:default-initargs :id (uuid:make-v4-uuid)))

(defmethod make-id ((self (eql :mpk))) (make-instance 'mpk-db-id))

;;; DB
(defclass mpk-db (rdb-database) ())

(defmethod make-db ((engine (eql :mpk)) &rest initargs)
  (declare (ignore engine))
  (change-class (apply 'make-db :rdb initargs) 'mpk-db))

(defmethod get-db (dbs (name (eql :mpk)))
  (find name dbs :key 'name :test 'string-equal))

(defun ensure-mpk-db ()
  (etypecase *db*
    (mpk-db *db*)
    (null (mpk-db-init))))

(defun mpk-db-init ()
  (if *db*
      (simple-rdb-warning "*DB* already bound.")
      (setq *db* 
            (make-db :mpk
              :name (namestring *mpk-db-meta-directory*))))
  (if (probe-file *mpk-db-meta-directory*)
      (progn
        (load-opts *db* :backfill t)
        (open-columns* *db*))
      (progn 
        (open-db *db*)
        (load-schema *db* *mpk-db-schema*)
        (create-columns *db*))))

(defun mpk-db-shutdown (&optional (wait t))
  (when *db* 
    (shutdown-db *db* :wait wait)
    (setf *db* nil)))

(defun mpk-db-info (&key (schema t) stats log metadata)
  (when schema
    (schema-from-rdb-column-families (columns *db*))))

(defun mpk-metadata-sst (&optional (meta *music-metadata*))
  (with-sst (s :file (namestring #l"mpk:cache;metadata.sst") :destroy t)
    (let ((i -1))
      (maphash 
       (lambda (k v) (declare (ignore v)) (put-kv s (make-kv (integer-to-octets (incf i) 32) (string-to-octets (namestring k)))))
       meta))))

(defun ingest-metadata (&optional (metadata #l"mpk:cache;metadata.sst"))
  (ingest-db *db* (list (namestring metadata)) :column :metadata))
