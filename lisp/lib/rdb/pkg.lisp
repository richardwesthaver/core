;;; rdb.lisp --- High-level RocksDB API

;; a thin ORM for working with RocksDB storage. 

;; Low-level bindings are in rocksdb.lisp.

;; Commentary:

;; Code:
(defpackage :rdb
  (:use :cl :std :rocksdb 
   :sb-alien :db :query :schema 
   :btree :store :stored :log 
   :io/static :dat/serde :dat/proto)
  (:import-from :sb-ext :string-to-octets :octets-to-string)
  (:export 
   ;; err
   :rdb-error
   :open-db-error
   :open-backup-engine-error
   :destroy-db-error
   :destroy-backup-engine-error
   :rdb-alien-error
   :rdb-user-error
   :put-kv-error
   :get-kv-error
   :handle-errptr
   ;; raw
   :make-rocksdb-options :default-rocksdb-options
   :open-db-raw
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
   :create-snapshot-raw :release-snapshot-raw
   :create-sst-writer-raw :finish-sst-writer-raw
   :destroy-sst-writer-raw :open-sst-writer-raw
   :sst-put-raw :sst-delete-raw :sst-delete-range-raw :sst-file-size-raw
   :sst-put-str-raw
   :open-sst-file :close-sst-file
   :cf-name-raw :cf-id-raw
   ;; proto
   :find-cf
   :put-cf-key :get-cf-key
   :push-sap :push-sap*
   :pull-sap :pull-sap*
   :backfill-opts :push-opts
   :get-opt :set-opt
   :push-cf :open-cfs
   :create-cf :create-cfs
   :multi-get :delete-key
   :delete-key-ts :put-key-ts
   :delete-key-range
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
   :ingest-db
   ;; sst
   :sst-file-writer :make-sst-file-writer
   :open-sst :finish-sst
   :destroy-sst :sst-file-size
   ;; obj
   :rdb :make-rdb :create-db
   :rdb-sap :rdb-name :rdb-cfs :rdb-opts
   :rdb-cf-metadata :make-rdb-cf-metadata
   :rdb-cf-metadata-name :rdb-cf-metadata-size
   :rdb-level-metadata :make-rdb-level-metadata
   :rdb-sst-file-metadata :make-rdb-sst-file-metadata
   :rdb-sst-file-metadata-p :rdb-cf-metadata-p
   :rdb-level-metadata-p
   :rdb-bytes :rdb-bytes-buffer :rdb-opts-sap
   :rdb-opts :make-rdb-opts
   :default-rdb-opts
   :rdb-cf :make-rdb-cf :create-cf
   :rdb-cf-sap :rdb-cf-name
   :rdb-iter :make-rdb-iter :rdb-iter-p
   :rdb-iter-sap
   ;; macs
   :with-errptr*
   :with-rdb
   :*temp-db-destroy*
   :with-temp-rdb
   :do-db
   :with-cf
   :do-cf
   :with-iter ;; generic
   :do-cfs
   :with-sst
   :nil
   :rdb-cf-p
   :copy-rdb-cf
   :rdb-cf-key-type
   :rdb-cf-val-type
   :close-cf
   :close-cfs
   :rdb-cf-opts
   :with-latest-opts
   :make-rdb-opts*
   :load-opts
   :with-open-rdb-raw
   :rdb-logger
   :close-backup-db
   :open-backup-db
   :open-transaction-db
   :close-transaction-db
   :read-opts
   :write-opts
   :close-secondary-db
   :open-secondary-db
   :rdb-transaction-db
   :rdb-transaction
   :rdb-secondary-db
   :rdb-backup-db
   :open-db-secondary-raw
   :create-checkpoint-raw
   :open-cfs-secondary-raw))

(in-package :rdb)
(rocksdb:load-rocksdb nil)
