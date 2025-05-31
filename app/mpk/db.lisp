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

(defvar *mpk-metadata-schema* (make-instance 'mpk-db-schema))

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

(defun mpk-db-init (&key (metadata t))
  (when *db* 
    (simple-rdb-warning "*DB* already bound")
    (shutdown-db *db*))
  (when metadata
    (setq *db*
          (make-db :mpk
            :name (namestring (mpk-db-path "metadata"))
            :opts (make-rdb-opts :allow-ingest-behind t :create-if-missing t)
            :logger (create-default-logger-callback)))
    (if (probe-file (mpk-db-path "metadata"))
        (progn
          (load-opts *db* :backfill t)
          (open-columns* *db*))
        (progn
          (open-db *db*)
          (load-schema *db* *mpk-metadata-schema*)
          (create-columns *db*)))))

(defun mpk-db-shutdown (&optional (wait t))
  (when *db* 
    (shutdown-db *db* :wait wait)
    (setf *db* nil)))

(defun mpk-db-info (&key (schema t) stats log metadata)
  (when schema
    (schema-from-rdb-column-families (columns *db*))))

(defun make-metadata-sst (&optional (meta *music-metadata*))
  (wait-for-threads
   (list
    (with-thread (:name "music-files")
      (with-sst (s :file (namestring #l"mpk:cache;file.sst") :destroy t)
        (let ((i -1))
          (maphash-keys 
           (lambda (k) 
             (put-kv s (make-kv (integer-to-octets (incf i) 128) (string-to-octets (namestring k)))))
           meta))))
    (with-thread (:name "music-names")
      (with-sst (s :file (namestring #l"mpk:cache;name.sst") :destroy t)
        (let ((i -1))
          (mapc (lambda (k) 
                  (put-kv 
                   s 
                   (make-kv 
                    (integer-to-octets (incf i) 128)
                    (if k
                        (string-to-octets k)
                        (make-octets 0)))))
                (get-music-metadata* "TITLE"))))))))

(defun insert-music-metadata (&key (file t) (name t))
  (let ((files (rdb::make-rdb-wbwi)) (names (rdb::make-rdb-wbwi)))
    (wait-for-threads
     (flatten
      (list
       (when file
         (with-thread (:name "music-files")
           (let ((i -1))
             (maphash-keys 
              (lambda (k) 
                (put-kv files (make-kv (integer-to-octets (incf i) 128) (string-to-octets (namestring k)))))
              *music-metadata*))))
       (when name
         (with-thread (:name "music-names")
           (let ((i -1))
             (mapc (lambda (k) 
                     (put-kv 
                      names 
                      (make-kv 
                       (integer-to-octets (incf i) 128)
                       (if k
                           (string-to-octets k)
                           (make-octets 0)))))
                   (get-music-metadata* "TITLE"))))))))
    (unwind-protect
         (progn
           (when file (rdb-write *db* files))
           (when name (rdb-write *db* names)))
      (when file (rocksdb::rocksdb-writebatch-wi-destroy (sap files)))
      (when name (rocksdb::rocksdb-writebatch-wi-destroy (sap names))))))

;; (defun update-metadata ())

;; WHY DOES THIS CORRUPT
(defun ingest-metadata-sst (&key (file #l"mpk:cache;file.sst") (name #l"mpk:cache;name.sst"))
  (let ((opts (rocksdb:rocksdb-ingestexternalfileoptions-create)))
    (rocksdb:rocksdb-ingestexternalfileoptions-set-allow-global-seqno opts nil)
    (rocksdb:rocksdb-ingestexternalfileoptions-set-move-files opts t)
    (ingest-db *db* (list (namestring file)) :column :file :opts opts)
    (ingest-db *db* (list (namestring name)) :column :name :opts opts)))

(defun get-metadata* (column)
  (rdb:with-column (cf (find-column column *db*))
    (std/seq:with-iter (it (iter *db* :cf cf))
      (seek-to-first it)
      (loop while (iter-valid-p it)
            do (progn
                 (fmt-row (list
                           (octets-to-integer (key it))
                           (sb-ext:octets-to-string (val it)))
                          t)
                 (next it))))))
