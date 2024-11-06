;;; db.lisp --- Skel Database Protocol

;; 

;;; Code:
(in-package :skel/db)

(defvar *skel-db-path* (merge-pathnames "db/" *skel-store*))

(defun skel-db-spec (path &optional (backend :rdb))
  "Return a list which can be safely stored in the SPEC slot of a STORE."
  (list backend (directory-path (merge-pathnames path *skel-db-path*))))

(defvar *default-skel-db-spec* (skel-db-spec "default"))

(defclass skel-db (database) ()
  (:default-initargs
   :db (make-rdb "skel-db" (default-rdb-opts))))

(defmethod name ((self skel-db)) (rdb-name (db self)))

(defmethod initialize-instance :after ((self skel-db) &rest initargs &key name)
  (declare (ignore initargs))
  (when name (setf (name self) name)))

(defmethod start :after ((self skel-db))
  (setq *db* self))

(defclass skel-store (store) ()
  (:default-initargs :spec *default-skel-db-spec*))

(defmethod start :after ((self skel-store))
  (setq *store* self))

(defclass skel-db-schema (database-schema) ()
  ((collection :type (vector schema) :initarg :collection :accessor schema-collection))
  (:default-initargs
   :name "skel-db"
   :fields ( ))

(defclass skel-object-schema (object-schema) ())

(defclass skel-record (id) ()
  (:metaclass stored-class))
(make-instance 'skel-record)
