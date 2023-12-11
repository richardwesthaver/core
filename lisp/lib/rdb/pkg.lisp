;;; rdb.lisp --- High-level RocksDB API

;; a thin ORM for working with RocksDB storage. 

;; Low-level bindings are in rocksdb.lisp.

;; Commentary:

;; Code:
(defpackage :rdb
  (:use :cl :std/alien :std/fu :std/sym :rocksdb)
  (:import-from :sb-ext :string-to-octets :octets-to-string)
  (:export 
   ;; err
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
   :put-kv :put-kv-str
   :get-kv :get-kv-str
   :put-cf :put-cf-str
   :get-cf :get-cf-str
   ;; obj
   :rdb :make-rdb :open-rdb :close-rdb :destroy-rdb
   :rdb-db :rdb-name :rdb-cfs :rdb-opts
   :push-cf :init-cfs
   :insert-kv :insert-kv-str
   :rdb-opts :make-rdb-opts
   :default-rdb-opts
   :rdb-cf :make-rdb-cf :create-cf
   :rdb-cf-sap :rdb-cf-name
   ;; macs
   :with-db
   :with-cf))
