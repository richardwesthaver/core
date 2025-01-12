;;; db.lisp --- Skel Database Protocol

;; 

;;; Code:
(in-package :skel/db)

(defvar *skel-db-path* (merge-homedir-pathnames ".stash/skel/db/"))
(defun skel-db-path (path) (merge-pathnames path *skel-db-path*))

(defun skel-db-spec (path &optional (backend :rdb))
  "Return a list which can be safely stored in the SPEC slot of a STORE."
  (list backend (directory-path (skel-db-path path))))

(defvar *default-skel-db-spec* (skel-db-spec "default"))

(defclass skel-db (rdb-database) 
  ()
  (:default-initargs
   :db (make-db :rocksdb :name "skel-db" :opts (default-rdb-opts))))

(defmethod make-db ((engine (eql :skel)) &rest initargs &key name path &allow-other-keys)
  (let ((name (or name (when path (namestring path)))))
    (remf initargs :name)
    (remf initargs :path)
    (let ((db (apply 'make-instance 'skel-db initargs)))
      (when name (setf (name db) name))
      db)))

(defaccessor (name) ((self skel-db)) (rdb-name (db self)))
(defaccessor (path) ((self skel-db)) (rdb-name (db self)))

(defmethod initialize-instance :before ((self skel-db) &rest initargs &key name path &allow-other-keys)
  (declare (ignore initargs))
  (unless name
    (unless (not path)
      (setf name (namestring path)))))

(defmethod start :after ((self skel-db))
  (setq *db* (open-db self)))

(defclass skel-store (store) ()
  (:default-initargs :spec *default-skel-db-spec*))

(defmethod start :after ((self skel-store))
  (setq *store* self))

(defclass skel-db-schema (upgradable-schema)
  ((collection :type (vector rdb-schema) :initarg :collection :accessor schema-collection))
  (:default-initargs
   :name "skel-db"))

(defclass skel-object-schema (object-schema) ())

(defclass skel-record (id) ()
  (:metaclass stored-class))

(defvar *skel-registry-db* (make-db :skel :path (skel-db-path "registry") :schema *skel-registry-schema*))
(defvar *skel-cache-db* (make-db :skel :path (skel-db-path "cache") :schema *skel-cache-schema*))
;; (with-db (db :db *skel-cache-db* :open t :close t))
