;;; query.lisp --- Query Support for RDB

;; RDB Queries based on Q/* and OBJ/QUERY packages.

;;; Code:
(in-package :rdb)

(defclass rdb-schema (schema) ())

(defclass rdb-data-source (data-source)
  ((db :type rdb :initarg :db :accessor db)
   (schema :type rdb-schema :initarg :schema :accessor schema)))

(defclass rdb-execution-context (execution-context) ())

(defclass rdb-query-plan (query-plan) ())

(defclass rdb-logical-plan (logical-plan) ())

(defclass rdb-physical-plan (physical-plan) ())

(defclass rdb-query (query) ())
