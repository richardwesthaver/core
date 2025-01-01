;;; pkg.lisp --- Skel Database

;; 

;;; Code:
(defpackage :skel/db
  (:use :cl :std :skel/core/condition 
   :skel/core/obj :skel/core/proto :skel/core/vars :db
   :store :schema :query :rdb
   :id :stored))

(in-package :skel/db)

(load-database-backend :rdb)
