;;; log.lisp --- RDB Logger

;; A LOGGER with a SINK that writes LOG-MESSAGEs to a RDB-DATABASE.

;;; Code:
(in-package :rdb)

(defclass rdb-sink (db-sink rdb-database) ()
  (:default-initargs
   :db (make-db :rdb)))

(defclass rdb-logger (database-logger) ())
