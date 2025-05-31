;;; log.lisp --- RDB Logger

;; RDB Logging Utils

;;; Commentary:

;; Log messages TO a db with RDB-SINK.

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
    :error '(octet-vector . string)))
  (:documentation "A LOGGER with a SINK that writes LOG-MESSAGEs to a RDB-DATABASE."))

(defclass rdb-sink (db-sink rdb-database) ()
  (:default-initargs
   :db (make-db :rdb)))

;; TODO 2025-05-30: 
(defclass rdb-logger (database-logger) ())
