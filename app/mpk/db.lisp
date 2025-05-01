;;; mpk/db.lisp --- MPK Database

;; Internal MPK Databases

;;; Commentary:

;; We initially started MPK with a single database attached - MDB (Media Database)

;; Since then our capabilities have grown and we need to manage many different
;; media types concurrently - A single RDB instance is too limiting for this.

;;; Code:
(in-package :mpk/db)

(defvar *mpk-db-backend-options* rdb::*rdb-backend-options*)
(set-database-backend :mpk *mpk-db-backend-options*
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

(defmethod make-id ((self (eql :mpk-db))) (make-instance 'mpk-db-id))

;;; DB
(defclass mpk-db (rdb-database) ())

(defmethod make-db ((engine (eql :mpk-db)) &rest initargs)
  (declare (ignore engine))
  (change-class (apply 'make-db :rdb initargs) 'mpk-db))

(defmethod get-db (dbs (name (eql :mpk-db)))
  (sb-sequence:find name dbs :key 'name :test 'string-equal))

(defun ensure-mpk-db ()
  (etypecase *db*
    (mpk-db *db*)
    (null (mpk-db-init))
    (t (rdb-error "*DB* is not of type MPK-DB."))))

(defun mpk-db-init ()
  (if *db*
      (simple-rdb-warning "*DB* already bound.")
      (setq *db* 
            (make-db :mpk-db
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
    
(defun schema-from-columns (columns)
  "Convert a sequence of COLUMNs to a SCHEMA."
  (let ((i 0))
    (apply 'make-schema 
	   (map 'list 
		(lambda (x)
		  (incf i)
		  (typecase x
		    (simple-column (make-field :name (keywordicate (name x)) :type (column-type x)))
		    (column (make-field :name i :type (column-type x)))))
		columns))))

(defun mpk-db-info (&key (schema t) stats log metadata)
  (when schema
    (schema-from-rdb-column-families (columns *db*))))
