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

(defvar *mpk-db-table* (make-hash-table))

(defun register-mpk-db (name db)
  (setf (gethash name *mpk-db-table*) db))

(set-database-backend 
 :mpk *mpk-db-backend-options*
 (lambda () (db::%load-database-backend :rdb)))

(defschema mpk-db-schema (rdb-schema)
  ((:id (uuid . string))))

(defschema mpk-meta-schema (mpk-db-schema)
  ((:file (uuid . string))
   (:format (uuid . octet))))

;; (funcall (caddr (assoc :fields (class-default-initargs (find-class 'mpk-meta-schema)))))

(defschema mpk-aud-meta-schema (mpk-meta-schema mpk-db-schema) ())

;; (make-instance 'mpk-music-meta-schema)

(defschema mpk-vid-meta-schema (mpk-meta-schema) ())

(defschema mpk-img-meta-schema (mpk-meta-schema) ())

(defschema mpk-music-meta-schema (mpk-aud-meta-schema)
  ((:title (uuid . string))))

(defschema mpk-movies-meta-schema (mpk-vid-meta-schema)
  ((:title (uuid . string))))

(defschema mpk-tv-meta-schema (mpk-vid-meta-schema)
  ((:title (uuid . string))))

(defschema mpk-screenshot-meta-schema (mpk-img-meta-schema) ())

(load-database-backend :mpk)

;;; Config
(defconfig mpk-db-config (db-config) ()
  (:default-initargs :backend :mpk))

(defmethod make-config ((self (eql :mpk-db)) &rest args &key &allow-other-keys)
  (apply 'make-instance 'mpk-db-config args))

;;; ID
(defclass mpk-db-id (id) ()
  (:default-initargs :id (uuid:make-v4-uuid)))

(defmethod make-id ((self (eql :mpk))) (make-instance 'mpk-db-id))

;;; DB
(defclass mpk-db (rdb-database) ())

(defmethod make-db ((engine (eql :mpk)) &rest initargs)
  (declare (ignore engine))
  (change-class 
   (apply 'make-db :rdb initargs)
   'mpk-db))

(defmethod find-db (name (dbs hash-table) &key)
  (gethash name dbs))

(defun ensure-mpk-db ()
  (etypecase *db*
    (mpk-db *db*)
    (null (mpk-db-init))))

(defun mpk-db-init ()
  (flet ((music.meta ()
           (let ((db (make-db :mpk
                       :path (namestring (mpk-db-path "meta/music"))
                       :opts (make-rdb-opts :allow-ingest-behind t :create-if-missing t)
                       :logger (create-default-logger-callback))))
             (register-mpk-db :music.meta db)
             (if (probe-file (mpk-db-path "meta/music"))
               (progn
                 (load-opts db :backfill t)
                 (open-columns* db))
               (progn
                 (open-db db)
                 (load-schema db (make-instance 'mpk-music-meta-schema))
                 (create-columns db)))))
         (movies.meta ()
           (let ((db (make-db :mpk
                       :path (namestring (mpk-db-path "meta/movies"))
                       :opts (make-rdb-opts :allow-ingest-behind t :create-if-missing t)
                       :logger (create-default-logger-callback))))
             (register-mpk-db :movies.meta db)
             (if (probe-file (mpk-db-path "meta/movies"))
                 (progn
                   (load-opts db :backfill t)
                   (open-columns* db))
                 (progn
                   (open-db db)
                   (load-schema db (make-instance 'mpk-movies-meta-schema))
                   (create-columns db)))))
         (tv.meta ()
           (let ((db (make-db :mpk
                       :path (namestring (mpk-db-path "meta/tv"))
                       :opts (make-rdb-opts :allow-ingest-behind t :create-if-missing t)
                       :logger (create-default-logger-callback))))
             (register-mpk-db :tv.meta db)
             (if (probe-file (mpk-db-path "meta/tv"))
                 (progn
                   (load-opts db :backfill t)
                   (open-columns* db))
                 (progn
                   (open-db db)
                   (load-schema db (make-instance 'mpk-tv-meta-schema))
                   (create-columns db)))))
         (img.meta ()
           (let ((db (make-db :mpk
                       :path (namestring (mpk-db-path "meta/img"))
                       :opts (make-rdb-opts :allow-ingest-behind t :create-if-missing t)
                       :logger (create-default-logger-callback))))
             (register-mpk-db :img.meta db)
             (if (probe-file (mpk-db-path "meta/img"))
                 (progn
                   (load-opts db :backfill t)
                   (open-columns* db))
                 (progn
                   (open-db db)
                   (load-schema db (make-instance 'mpk-img-meta-schema))
                   (create-columns db)))))
         (aud.meta ()
           (let ((db (make-db :mpk
                       :path (namestring (mpk-db-path "meta/aud"))
                       :opts (make-rdb-opts :allow-ingest-behind t :create-if-missing t)
                       :logger (create-default-logger-callback))))
             (register-mpk-db :aud.meta db)
             (if (probe-file (mpk-db-path "meta/aud"))
                 (progn
                   (load-opts db :backfill t)
                   (open-columns* db))
                 (progn
                   (open-db db)
                   (load-schema db (make-instance 'mpk-aud-meta-schema))
                   (create-columns db)))))
         (vid.meta ()
           (let ((db (make-db :mpk
                       :path (namestring (mpk-db-path "meta/vid"))
                       :opts (make-rdb-opts :allow-ingest-behind t :create-if-missing t)
                       :logger (create-default-logger-callback))))
             (register-mpk-db :vid.meta db)
             (if (probe-file (mpk-db-path "meta/vid"))
                 (progn
                   (load-opts db :backfill t)
                   (open-columns* db))
                 (progn
                   (open-db db)
                   (load-schema db (make-instance 'mpk-vid-meta-schema))
                   (create-columns db)))))
         (screenshot.meta ()
           (let ((db (make-db :mpk
                       :path (namestring (mpk-db-path "meta/screenshot"))
                       :opts (make-rdb-opts :allow-ingest-behind t :create-if-missing t)
                       :logger (create-default-logger-callback))))
             (register-mpk-db :screenshot.meta db)
             (if (probe-file (mpk-db-path "meta/screenshot"))
                 (progn
                   (load-opts db :backfill t)
                   (open-columns* db))
                 (progn
                   (open-db db)
                   (load-schema db (make-instance 'mpk-screenshot-meta-schema))
                   (create-columns db))))))
    (when *db* 
      (shutdown-db *db*))
    (ensure-directories-exist (mpk-db-path "meta/"))
    (music.meta)
    (movies.meta)
    (tv.meta)
    (img.meta)
    (aud.meta)
    (vid.meta)
    (screenshot.meta)))

(defun mpk-db-shutdown (&optional (wait t))
  (maphash-values (lambda (v) (shutdown-db v :wait wait)) *mpk-db-table*))

(defun mpk-db-info (db &key (schema t)) ;; stats log metadata)
  (when schema
    (schema-from-rdb-column-families (columns (find-db db *mpk-db-table*)))))

(defun update-music-metadata ()
  (with-db (*db* :db (find-db :music.meta *mpk-db-table*) :open nil :close nil)
    (with-wbwi (b)
      (let ((file-cf (find-column :file *db*)) (name-cf (find-column :title *db*)) (id-cf (find-column :id *db*)))
        (maphash-keys
         (lambda (k)
           (when-let* ((k k)
                       (id (uuid:uuid-to-octet-vector (uuid:make-v1-uuid)))
                       (file (string-to-octets (namestring k)))
                       (title (get-music-metadata k :title)))
             (wbwi-put-kv-cf
              b id-cf
              (make-kv id file))
             (wbwi-put-kv-cf 
              b file-cf 
              (make-kv file id))
             (wbwi-put-kv-cf
              b name-cf
              (make-kv id (string-to-octets title)))))
         *music-metadata*)
        (rdb-write *db* b)))))

;; (defun update-movies-metadata ())

;; CORRUPTED - MAGIC TABLE #
(defun ingest-metadata-sst (&key (file #l"mpk:cache;file.sst") (name #l"mpk:cache;name.sst"))
  (let ((opts (rocksdb:rocksdb-ingestexternalfileoptions-create)))
    (rocksdb:rocksdb-ingestexternalfileoptions-set-allow-global-seqno opts nil)
    (rocksdb:rocksdb-ingestexternalfileoptions-set-move-files opts t)
    (ingest-db *db* (list (namestring file)) :column :file :opts opts)
    (ingest-db *db* (list (namestring name)) :column :title :opts opts)))

(defun get-metadata* ()
  (with-db (*db* :db (find-db :music.meta *mpk-db-table*) :open nil :close nil)
    (with-column (name (find-column :title *db*))
      (with-iter (it (iter *db* :column :id))
        (seek-to-first it)
        (loop while (iter-valid-p it)
              do (let ((k (skey it))
                       (v (sval it)))
                   (fmt-row (list
                             (uuid::octet-vector-to-uuid k)
                             (unless (null v)
                               (sb-ext:octets-to-string v))
                             (when-let ((n (val:get-val *db* k :column name)))
                               (sb-ext:octets-to-string n)))
                            t)
                   (next it)))))))
