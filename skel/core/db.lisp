;;; db.lisp --- Skel Database Protocol

;; 

;;; Code:
(in-package :skel/core)
(load-alien :rocksdb)

(defvar *skel-registry-schema*
  (make-instance 'simple-schema
    :fields 
    (make-fields 
     :id '(fixnum . string)
     :properties '(fixnum . octet-vector)
     :name '(string . fixnum)
     :path '(fixnum . pathname)
     :tags '(fixnum . (array string)))))

(defvar *skel-cache-schema*
  (make-instance 'simple-schema
    :fields 
    (make-fields
     :id '(fixnum . octet-vector)
     :hash '((octet-vector 32) . fixnum)
     :updated '(fixnum . octet-vector))))

(defvar *skel-db-path* (merge-homedir-pathnames ".stash/skel/db/"))

(defun skel-db-path (path) (merge-pathnames path *skel-db-path*))

(defun skel-db-spec (path &optional (backend :rdb))
  "Return a list which can be safely stored in the SPEC slot of a STORE."
  (list backend (directory-path (skel-db-path path)) (default-rocksdb-options)))

(defvar *default-skel-db-spec* (skel-db-spec "default"))

(defclass skel-db (rdb) 
  ()
  (:default-initargs
   :db (make-db :rocksdb :name "skel-db" :opts (default-rocksdb-options))))

(defmethod make-db ((engine (eql :skel)) &rest initargs &key name path &allow-other-keys)
  (let ((name (or name (when path (namestring path)))))
    (remf initargs :name)
    (remf initargs :path)
    (let ((db (apply 'make-instance 'skel-db initargs)))
      (when name (setf (name db) name))
      db)))

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

(defclass skel-record (id) ()
  (:metaclass stored-class))
;; (make-db :skel :path (skel-db-path "registry") :schema *skel-registry-schema*)
(defvar *skel-registry-db*)
;; (make-db :skel :path (skel-db-path "cache") :schema *skel-cache-schema*)
(defvar *skel-cache-db*)
;; (with-db (db :db *skel-cache-db* :open t :close t))
