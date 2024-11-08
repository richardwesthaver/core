;;; db.lisp --- Packy Database

;; 

;;; Code:
(in-package :packy/db)

(defclass package-database (database) ()
  (:default-initargs 
   :db (make-rdb "packy" (default-rdb-opts) #())))

(defmethod make-db ((engine (eql :packy)) &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'package-database initargs))

(defmethod connect-db ((db package-database) &key &allow-other-keys)
  (open-db (db db)))

(defmethod query-db ((db package-database) query &key &allow-other-keys))
  
(defmethod db-get ((db package-database) (key simple-string) &key &allow-other-keys)
  ;; lol
    (with-db (db (db db))
      (get-kv-str-raw db key)))

(defmethod close-db ((db package-database) &key &allow-other-keys)
  (close-db (db db)))

(defmethod destroy-db ((db package-database))
  (destroy-db (db db)))

(defmethod get-val ((obj package-database) (elt simple-string) &key data-type)
  (declare (ignore data-type))
  (db-get obj elt))

(defmethod get-db (dbs (name (eql :packy))))
