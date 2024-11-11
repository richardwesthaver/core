;;; log.lisp --- RDB Logger

;; A Logger which writes to a RDB instance.

;;; Code:
(in-package :rdb)

(defclass rdb-logger (database-logger rdb-database) ())
