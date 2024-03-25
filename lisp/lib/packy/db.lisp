(in-package :packy/core)

(defclass packy-database (database) ()
  (:default-initargs 
   :db (make-rdb "packy" (default-rdb-opts) #())))

(defmethod make-db ((engine (eql :packy)) &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'packy-database initargs))

(defmethod connect-db ((db packy-database) &key &allow-other-keys)
  (open-db (db db)))

(defmethod db-query ((db packy-database) query &key &allow-other-keys))
  
(defmethod db-get ((db packy-database) (key simple-string) &key &allow-other-keys)
  ;; lol
    (with-db (db (db db))
      (get-kv-str-raw db key)))

(defmethod close-db ((db packy-database) &key &allow-other-keys)
  (close-db (db db)))

(defmethod destroy-db ((db packy-database))
  (destroy-db (db db)))

(defmethod get-val ((obj packy-database) (elt simple-string) &optional data-type)
  (declare (ignore data-type))
  (db-get obj elt))

(defmethod get-db (dbs (name (eql :packy))))
