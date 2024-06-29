;;; query.lisp --- Query Support for RDB

;; RDB Queries based on Q/* and OBJ/QUERY packages.

;;; Code:
(in-package :rdb)

(defclass rdb-data-source (data-source)
  ((db :type rdb :initarg :db :accessor db)))
