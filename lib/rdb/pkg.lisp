;;; rdb.lisp --- High-level RocksDB API

;; a thin ORM for working with RocksDB storage. 

;; Low-level bindings are in rocksdb.lisp.

;; Commentary:

;; Code:
(rocksdb:load-rocksdb)
(defpkg :rdb
  (:use :cl :std :rocksdb 
   :sb-alien :db :schema :btree 
   :store :stored :log :io
   :time :config :ast 
   :id :std/seq :srv :net/srv :val :q/proto)
  (:import-from :db :backend :options)
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
   :handle-errptr
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
   :flush-cf
   :get-stats
   :print-stats
   :ingest-db
   ;; sst
   :sst-file-writer :make-sst-file-writer
   :open-sst :finish-sst
   :destroy-sst :sst-file-size
   ;; obj
   :rdb :make-rdb
   :rdb-sap :rdb-name :rdb-cfs
   :rdb-cf-metadata :make-rdb-cf-metadata
   :rdb-cf-metadata-name :rdb-cf-metadata-size
   :rdb-level-metadata :make-rdb-level-metadata
   :rdb-sst-file-metadata :make-rdb-sst-file-metadata
   :rdb-sst-file-metadata-p :rdb-cf-metadata-p
   :rdb-level-metadata-p
   :default-rocksdb-options
   :rdb-cf :make-rdb-cf :create-cf
   :rdb-cf-sap :rdb-cf-name
   :rdb-iter :make-rdb-iter :rdb-iter-p
   ;; macs
   :with-errptr*
   :with-rdb
   :*temp-db-destroy*
   :with-temp-rdb
   :with-sst
   :rdb-cf-p
   :copy-rdb-cf
   :rdb-cf-key-type
   :rdb-cf-val-type
   :close-cf
   :close-cfs
   :rdb-cf-opts
   :with-latest-opts
   :make-rocksdb-options
   
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
   :rdb-backup-engine
   :with-kv-raw
   :get-kv-error
   :put-kv-error
   :kv-error
   :cf-error
   :db-missing
   :metadata-missing
   :put-kv-cf-error
   :get-kv-cf-error
   :rdb-transaction-error
   :with-txn-raw
   :index-merge-op
   :concat-merge-op
   :rdb-log-default
   :do-columns
   :with-column
   :rdb-column-family
   :rdb-column
   :rdb-schema
   :create-concat-merge-op
   :create-index-merge-op
   :create-fixed-prefix-op
   :cf
   :create-default-logger-callback
   :rdb-database
   :with-wbwi
   :rdb-wbwi
   :rdb-wbwi-count
   :rdb-wbwi-data
   :rdb-wbwi-clear
   :rdb-wbwi-save
   :rdb-wbwi-ts
   :rdb-backup-engine-info
   :open-backup-engine
   :close-backup-engine
   :rdb-config
   :rdb-checkpoint
   :open-checkpoint-db
   :close-checkpoint-db
   :rdb-object-schema
   :rdb-data-source
   :rdb-sink
   :rdb-log-schema
   :cursor-handle
   :simple-rdb-warning
   :schema-from-rdb-column-families
   :rdb-write
   :init-rdbrc
   :rdb-service))

(defpkg :rdb/cli
  (:use :cl :std :cli :clap :rdb :db :std/seq))
