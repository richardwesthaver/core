;;; log.lisp --- RDB Logger

;; A LOGGER with a SINK that writes LOG-MESSAGEs to a RDB-DATABASE.

;;; Code:
(in-package :rdb)

(defclass rdb-log-schema (rdb-schema) ()
  (:default-initargs
   :fields 
   (make-fields 
    ;; log levels
    :trace '(octet-vector . string)
    :debug '(octet-vector . string)
    :info '(octet-vector . string)
    :warn '(octet-vector . string)
    :error '(octet-vector . string))))

(defclass rdb-sink (db-sink rdb-database) ()
  (:default-initargs
   :db (make-db :rdb)))

(defclass rdb-logger (database-logger) ())
