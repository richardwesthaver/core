;;; rdb.lisp --- High-level RocksDB API

;; a thin ORM for working with RocksDB storage. 

;; Low-level bindings are in rocksdb.lisp.

;; Commentary:

;; Code:
(defpackage :rdb
  (:use :cl :std :rocksdb :sb-alien :obj/db :obj/query)
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
   :create-snapshot-raw :release-snapshot-raw
   :create-sst-writer-raw :finish-sst-writer-raw
   :destroy-sst-writer-raw :open-sst-writer-raw
   :sst-put-raw :sst-delete-raw :sst-delete-range-raw :sst-file-size-raw
   :sst-put-str-raw
   :open-sst-file :close-sst-file
   :cf-name-raw :cf-id-raw
   ;; proto
   :find-cf
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
   :load-opts))

(in-package :rdb)
(rocksdb:load-rocksdb nil)
