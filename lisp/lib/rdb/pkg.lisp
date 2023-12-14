;;; rdb.lisp --- High-level RocksDB API

;; a thin ORM for working with RocksDB storage. 

;; Low-level bindings are in rocksdb.lisp.

;; Commentary:

;; Code:
(defpackage :rdb
  (:use :cl :std :rocksdb :sb-alien)
  (:import-from :sb-ext :string-to-octets :octets-to-string)
  (:export 
   ;; err
   :with-errptr
   :open-db-error
   :put-kv-error
   :get-kv-error
   ;; util
   :open-db-raw :with-open-db-raw
   :close-db-raw :destroy-db-raw
   :put-kv-raw :put-kv-str-raw
   :get-kv-raw :get-kv-str-raw
   :put-cf-raw :put-cf-str-raw
   :get-cf-raw :get-cf-str-raw
   :create-iter :with-iter
   :iter-key :iter-key-str
   :iter-val :iter-val-str
   ;; proto
   :put-key :put-kv
   :get-key :get-kv
   :put-cf-key :get-cf-key
   :push-sap :push-sap*
   :get-opt :set-opt
   :push-cf :init-db
   :insert-kv :insert-key
   :make-db :open-db :close-db :destroy-db
   ;; obj
   :rdb :make-rdb 
   :rdb-db :rdb-name :rdb-cfs :rdb-opts
   :rdb-bytes :rdb-bytes-buffer
   :make-rdb-key :make-rdb-kv :make-rdb-val :rdb-kv :rdb-key :rdb-val 
   :rdb-opts :make-rdb-opts
   :default-rdb-opts
   :rdb-cf :make-rdb-cf :create-cf
   :rdb-cf-sap :rdb-cf-name
   ;; macs
   :with-db
   :with-cf))
