;;; pkg.lisp --- Skel Database

;; 

;;; Code:
(defpackage :skel/db
  (:use :cl :std :skel/core/condition 
   :skel/core/obj :skel/core/proto :skel/core/var :db
   :store :schema :query :rdb
   :id :stored :log :config :build)
  (:export :sk-object-schema 
   :sk-schema :skel-db 
   :skel-db-path :*skel-registry-schema* 
   :*skel-cache-schema*
   :merge-homedir-pathnames
   :sk-log-schema
   :*skel-log-schema*
   :skel-db-logger
   :*skel-logger-config*
   :*skel-logger*
   :init-skel-db-logger))

(in-package :skel/db)

(load-database-backend :rdb)
