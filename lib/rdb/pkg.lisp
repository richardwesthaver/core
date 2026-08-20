;;; rdb.lisp --- High-level RocksDB API

;; a thin ORM for working with RocksDB storage. 

;; Low-level bindings are in rocksdb.lisp.

;; Commentary:

;; Code:
(defpkg :rdb
  (:use :cl :std :rocksdb 
   :sb-alien :db :schema :btree 
   :store :stored :log :io
   :time :config :ast 
   :id :std/seq :srv :net/srv :val :q/proto)
  (:import-from :db :options)
  (:import-from :sys :io-vector-class)
  (:import-from :sb-ext :string-to-octets :octets-to-string)
  (:export 
   :rdb-error
   :open-db-error
   :open-backup-engine-error
   :destroy-db-error
   :destroy-backup-engine-error
   :rdb-alien-error
   :handle-errptr
   :find-cf
   :put-cf-key :get-cf-key
   :multi-get :delete-key
   :flush-cf
   :get-stats
   :print-db-stats
   :ingest-db
   :sst-file-writer :make-sst-file-writer
   :rdb
   :srdb
   :rdb-metadata
   :level-metadata
   :sst-file-metadata
   :default-rocksdb-options
   :rdb-iter
   ;; macs
   :with-errptr*
   :with-rdb
   :*temp-db-destroy*
   :with-temp-rdb
   :with-sst
   :with-latest-opts
   :make-rocksdb-options
   :load-opts
   :with-open-rdb-raw
   :rdb-logger
   :close-secondary-db
   :open-secondary-db
   :trdb
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
   :column-family
   :simple-column-family
   :rdb-column
   :close-columns
   :rdb-schema
   :rdb-store
   :create-concat-merge-op
   :create-index-merge-op
   :create-fixed-prefix-op
   :create-default-logger-callback
   :open-all-columns
   :open-with-columns
   :with-wbwi
   :rdb-wbwi
   :rdb-wbwi-count
   :rdb-wbwi-data
   :rdb-wbwi-clear
   :rdb-wbwi-save
   :rdb-wbwi-ts
   :rdb-backup-engine-info
   :rdb-config
   :checkpoint
   :close-checkpoint
   :rdb-object-schema
   :rdb-data-source
   :rdb-sink
   :rdb-log-schema
   :simple-rdb-warning
   :schema-from-simple-column-families
   :rdb-write
   :init-rdbrc
   :rdb-service
   :get-base-db
   :list-column-families))

(defpkg :rdb/cli
  (:use :cl :std :cli :clap :rdb :db :std/seq))
