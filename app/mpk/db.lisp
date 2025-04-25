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

(defvar *mpk-db-directory* (mpk-data-path "db/"))
(defvar *mpk-db-meta-directory* (mpk-data-path "db/meta"))

(defvar *mpk-db-id-seed* (random 99999))

(defvar *mpk-db-schema* 
  (defschema rdb-schema (simple-schema)
    ((:id '(uuid . string))
     (:file '(uuid . string))
     (:name '(uuid . string))
     (:source '(uuid . string))
     (:state '(uuid . octet))
     (:meta '(uuid . string)))))

(load-database-backend :rdb)

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

(defun mpk-db-init ()
  (if *db*
      (simple-rdb-warning "*DB* already bound.")
      (setq *db* 
            (make-db :mpk-db
              :name (namestring *mpk-db-directory*))))
  (if (probe-file *mpk-db-directory*)
      (progn
        (load-opts *db* :backfill t)
        (open-db *db*))
      (progn 
        (open-db *db*)
        (load-schema *db* *mpk-db-schema*)
        (create-columns *db*))))
