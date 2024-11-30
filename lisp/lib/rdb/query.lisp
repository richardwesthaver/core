;;; query.lisp --- Query Support for RDB

;; RDB Queries based on Q/* and OBJ/QUERY packages.

;;; Code:
(in-package :rdb)

(defclass rdb-query-engine (query-engine rdb-database) ())

(defclass rdb-data-source (data-source)
  ((db :type rdb-database :initarg :db :accessor db)
   (schema :type rdb-schema :initarg :schema :accessor schema)))

(defmethod initialize-instance :after ((self rdb-data-source) &key)
  (unless (or (slot-boundp self 'schema) (not (slot-boundp self 'db)))
    (setf (schema self) (schema (db self)))))

(defclass rdb-execution-context (execution-context) ())

(defclass rdb-query-plan (query-plan) ())

(defclass rdb-logical-plan (logical-plan) ())

(defclass rdb-physical-plan (physical-plan) ())

(defclass rdb-query (query) ())
