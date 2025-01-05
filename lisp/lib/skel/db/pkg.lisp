;;; pkg.lisp --- Skel Database

;; 

;;; Code:
(defpackage :skel/db
  (:use :cl :std :skel/core/condition 
   :skel/core/obj :skel/core/proto :skel/core/vars :db
   :store :schema :query :rdb
   :id :stored)
  (:export :sk-object-schema 
   :sk-schema :skel-db 
   :skel-db-path :*skel-registry-schema* 
   :*skel-cache-schema*))

(in-package :skel/db)

(load-database-backend :rdb)
