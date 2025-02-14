;;; query.lisp --- Query Support for RDB

;; RDB Queries based on Q/* and OBJ/QUERY packages.

;;; Code:
(in-package :rdb)

(defclass rdb-query-engine (query-engine rdb-database) ())

(defclass rdb-execution-context (execution-context) ()
  (:documentation "A context in which RDB queries may be
  executed. DATA-FRAMEs, DATA-SOURCEs, SCHEMAs, and other complex objects are
  stored in the slots of this class and dynamically bound during execution
  phases."))

(defclass rdb-query-plan (query-plan) ())

(defclass rdb-query (query) ())
