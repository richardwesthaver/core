;;; schema.lisp --- RDB Schema Implementation

;; 

;;; Code:
(in-package :rdb)

(defschema rdb-schema (schema)
  ((:default (octet-vector . octet-vector)))
  (:documentation "A schema which may be read from a simple s-expression and ingested by an
RDB instance via LOAD-SCHEMA."))

(defschema rdb-object-schema (object-schema) ()
  (:documentation "An object schema which may be ingested by a RDB-STORE."))

;; Note that we don't use SIMPLE-CONS-COLUMN here because the NAME slot of
;; columns is stored in the underlying CF handle.
(defclass rdb-column (cons-column) ()
  (:default-initargs 
   :type (cons 'octet-vector 'octet-vector)))

(defun field-from-cf (cf)
  (make-field :name (%cf-name cf)
              :type (cons 'octet-vector 'octet-vector)))

(defclass rdb-data-source (data-source)
  ((db :type rdb :initarg :db :accessor db)
   (schema :type rdb-schema :initarg :schema :accessor schema)))

(defmethod initialize-instance :after ((self rdb-data-source) &key)
  (unless (or (slot-boundp self 'schema) (not (slot-boundp self 'db)))
    (setf (schema self) (schema (db self)))))
