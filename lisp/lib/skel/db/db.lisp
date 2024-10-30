;;; db.lisp --- Skel Database Protocol

;; 

;;; Code:
(in-package :skel/db)

(defclass skel-db (database) ()
  (:default-initargs
   :db (make-rdb "skel-db" (default-rdb-opts))))

(defmethod name ((self skel-db)) (rdb-name (db self)))

(defmethod initialize-instance :after ((self skel-db) &rest initargs &key name)
  (declare (ignore initargs))
  (when name (setf (name self) name)))

(defclass skel-store (store) ())

(defclass skel-schema (simple-schema) ())

(defclass skel-object-schema (object-schema) ())

(defclass skel-record (id) ()
  (:metaclass stored-class))
