;;; mpk/db.lisp --- Media Database

;; 

;;; Code:
(in-package :mpk/db)

(defvar *mdb-backend-options* rdb::*rdb-backend-options*)
(set-database-backend :mdb *mdb-backend-options*
                      (lambda () (db::%load-database-backend :rdb)))

(defvar *mdb-directory* (mpk-data-path "db/"))
(defvar *mdb-meta-directory* (mpk-data-path "db/meta"))
(defvar *mdb* nil)

(load-database-backend :rdb)

(defvar *mdb-id-seed* (random 99999))

(defclass mdb-id (id:id) ()
  (:default-initargs :id (uuid:make-v4-uuid)))

(defclass mdb (rdb-database) ()
  (:default-initargs
   :db (make-db :rocksdb
                :name (namestring *mdb-directory*)
                :opts (default-rdb-opts)
                :logger (rdb-log-default 1))))

(defmethod id:make-id ((self (eql :mdb))) (make-instance 'mdb-id))

(defmethod make-db ((engine (eql :mdb)) &rest initargs &key)
  (apply #'make-instance 'mdb initargs))

(defmethod get-db (dbs (name (eql :mdb))))

(defvar *mdb-schema* (make-instance 'rdb-schema
                       :fields (make-fields 
                                :id '(uuid . string)
                                :file '(uuid . string)
                                :name '(uuid . string)
                                :source '(uuid . string)
                                :state '(uuid . octet)
                                :meta '(uuid . string))))

(defun init-mdb ()
  (ifret *mdb*
    (setq *mdb* 
          (make-db :mdb
                   :opts (default-rdb-opts)
                   :name (namestring *mdb-directory*)))
    (if (probe-file *mdb-directory*)
        (progn
          (load-opts *mdb* :backfill t)
          (open-db *mdb*))
        (progn 
          (open-db *mdb*)
          (load-schema *mdb* *mdb-schema*)
          (create-columns *mdb*)))))

;; (setq *mdb* nil)
;; (init-mdb)
;; (columns *mdb*)
;; (create-columns *mdb*)
;; (open-columns* *mdb*)
