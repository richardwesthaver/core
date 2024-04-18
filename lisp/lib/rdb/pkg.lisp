;;; rdb.lisp --- High-level RocksDB API

;; a thin ORM for working with RocksDB storage. 

;; Low-level bindings are in rocksdb.lisp.

;; Commentary:

;; Code:
(defpackage :rdb
  (:use :cl :std :rocksdb :sb-alien :obj/db)
  (:import-from :sb-ext :string-to-octets :octets-to-string)
  (:export 
   ;; err
   :with-errptr
   :rdb-error
   :open-db-error
   :open-backup-engine-error
   :destroy-db-error
   :destroy-backup-engine-error
   :rocksdb-error
   :rdb-user-error
   :put-kv-error
   :get-kv-error
   :handle-errptr
   ;; raw
   :make-rocksdb-options :default-rocksdb-options
   :open-db-raw :with-open-db-raw
   :close-db-raw :destroy-db-raw
   :put-kv-raw :put-kv-str-raw
   :get-kv-raw :get-kv-str-raw
   :put-cf-raw :put-cf-str-raw
   :get-cf-raw :get-cf-str-raw
   :create-cf-raw
   :create-iter :with-iter-raw
   :iter-key-raw :iter-key-str-raw
   :iter-val-raw :iter-val-str-raw
   :open-backup-engine-raw
   :close-backup-engine-raw
   :create-new-backup-raw
   :restore-from-latest-backup-raw
   :with-open-backup-engine-raw
   ;; proto
   :put-key :put-kv
   :get-key :get-kv
   :put-cf-key :get-cf-key
   :push-sap :push-sap*
   :pull-sap :pull-sap*
   :backfill-opts :push-opts
   :get-opt :set-opt
   :push-cf :open-cfs
   :create-cf :create-cfs
   :insert-kv :insert-key
   :open-db :close-db :destroy-db
   :get-prop :get-metadata
   :multi-get :delete-key
   :make-transaction :commit-transaction
   :begin-transaction :prepare-transaction
   :rollback-transaction :delete-transaction
   :flush-db :flush-cf
   :repair-db :write-db
   :backup-db :restore-db
   :get-stats :snapshot-db
   :shutdown-db :print-stats
   :create-iter :iter-next
   :iter-prev :iter-seek
   :iter-key :iter-val
   :iter-timestamp :iter-kv
   :iter-seek-to-first
   :iter-seek-to-last
   :iter-seek-for-prev
   :iter-valid-p
   ;; sst
   :sst-file
   :sst-stream
   ;; obj
   :rdb :make-rdb :create-db
   :rdb-db :rdb-name :rdb-cfs :rdb-opts
   :rdb-cf-metadata :make-rdb-cf-metadata
   :rdb-cf-metadata-name :rdb-cf-metadata-size
   :rdb-level-metadata :make-rdb-level-metadata
   :rdb-sst-file-metadata :make-rdb-sst-file-metadata
   :rdb-sst-file-metadata-p :rdb-cf-metadata-p
   :rdb-level-metadata-p
   :rdb-bytes :rdb-bytes-buffer :rdb-opts-sap
   :make-key :make-kv :make-val :rdb-kv :rdb-key :rdb-val 
   :rdb-kv
   :rdb-opts :make-rdb-opts
   :default-rdb-opts
   :rdb-cf :make-rdb-cf :create-cf
   :rdb-cf-sap :rdb-cf-name
   :rdb-iter :make-rdb-iter :rdb-iter-p
   :rdb-iter-sap
   ;; macs
   :with-errptr
   :with-db
   :*temp-db-destroy*
   :with-temp-db
   :do-db
   :with-cf
   :do-cf
   :with-iter ;; generic
   :do-cfs))

(in-package :rdb)
(rocksdb:load-rocksdb t)
