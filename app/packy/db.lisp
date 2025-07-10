;;; db.lisp --- Packy Database

;; 

;;; Code:
(in-package :packy)

(defvar *packy-backend-options* rdb::*rdb-backend-options*)

(set-database-backend :packy *packy-backend-options*
                      (lambda () (db::%load-database-backend :rdb)))

(defclass packy-schema (rdb-schema) ()
  (:default-initargs
   :fields (make-fields :id '(fixnum . string)
                        :name '(string . fixnum)
                        :hash '(fixnum . (octet-vector 32))
                        :installed '(fixnum . octet-vector)
                        :updated '(fixnum . octet-vector)
                        :path '(fixnum . pathname)
                        :type '(fixnum . octet)
                        :tags '(fixnum . (array string))
                        :uri '(fixnum . string))))

(defclass packy-db (rdb-database) ()
  (:default-initargs 
   :db (make-db :rocksdb 
                :name (namestring (merge-pathnames "db/" *packy-home*))
                :opts (default-rdb-opts)
                :logger (rdb-log-default 10))))

(defmethod make-db ((engine (eql :packy)) &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'packy-db initargs))

(defmethod find-db ((name (eql :packy)) dbs &key))

(defun init-packy-db ()
  (setq *packy-db* (load-schema (make-db :packy) (make-instance 'packy-schema))))

(defun insert-pack (pack)
  (make-instance 'pack))
