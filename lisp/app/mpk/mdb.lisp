;;; mdb.lisp --- Media DataBase

;; 

;;; Code:
(in-package :mdb)
(defvar *mdb-directory* (mpk-path "mdb/"))
(defvar *mdb* nil)
(defvar *mdb-schema* (make-instance 'rdb-schema
                       :fields (make-fields :id '(fixnum . fixnum))))
(load-database-backend :rdb)

(defun mdb-init ()
  (ifret *mdb*
    (with-db (db :db (make-db :rdb :opts (default-rdb-opts) :path *mdb-directory*) :open t)
      (setq *mdb* 
            (open-db 
             (load-schema 
              db
              *mdb-schema*)))
      (create-columns *mdb*))))
