;;; log.lisp --- RDB Logger

;; RDB Logging Utilities

;;; Commentary:

;; Log messages to a dedicated logging db with RDB-SINK and RDB-LOGGER. The
;; RDB-LOG-SCHEMA class defines the required fields - one column-family per
;; log level.

;; For working with the logging mechanism within RocksDB itself, several
;; utility functions are provided with the RDB-LOG prefix.

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

;; TODO 2025-05-30: 
(defclass rdb-logger (database-logger) ()
  (:documentation "A LOGGER with a SINK that writes LOG-MESSAGEs to a RDB-DATABASE."))

;;; RDB-LOG
(defun rdb-log-default (level &optional prefix)
  (if prefix
      (rocksdb-logger-create-stderr-logger level prefix)
      (rocksdb-logger-create-callback-logger 
       level 
       (alien-sap (alien-callable-function 'rocksdb-log-default))
       nil)))
