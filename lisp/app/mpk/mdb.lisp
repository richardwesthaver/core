;;; mdb.lisp --- Media DataBase

;; 

;;; Code:
(in-package :mdb)
(defvar *mdb-directory* (mpk-path "mdb/"))

(defvar *mdb-schema* (make-instance 'rdb-schema
                       :fields (make-fields :id '(fixnum . fixnum))))

(defclass mdb (rdb-database) ())
