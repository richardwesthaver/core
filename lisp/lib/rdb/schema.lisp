;;; schema.lisp --- RDB Schema Implementation

;; 

;;; Code:
(in-package :rdb)

(defclass rdb-schema (schema) ()
  (:default-initargs :fields (make-fields :default '(octet-vector . octet-vector))))

(defclass rdb-object-schema (object-schema) ())

;; Note that we don't use SIMPLE-CONS-COLUMN here because the NAME slot of
;; columns is stored in the underlying RDB-CF structure object.
(defclass rdb-column (cons-column) ()
  (:default-initargs 
   :type (cons 'octet-vector 'octet-vector)))

(defun cf-to-field (cf)
  (make-field :name (name cf)
              :type (cons 'octet-vector 'octet-vector)))

(defclass rdb-data-source (data-source)
  ((db :type rdb-database :initarg :db :accessor db)
   (schema :type rdb-schema :initarg :schema :accessor schema)))

(defmethod initialize-instance :after ((self rdb-data-source) &key)
  (unless (or (slot-boundp self 'schema) (not (slot-boundp self 'db)))
    (setf (schema self) (schema (db self)))))
