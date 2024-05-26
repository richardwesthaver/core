(in-package :packy/core)

(defclass packy-db (database) ()
  (:default-initargs 
   :db (make-rdb "packy" (default-rdb-opts) #())))

(defmethod make-db ((engine (eql :packy)) &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'packy-db initargs))

(defmethod connect-db ((db packy-db) &key &allow-other-keys)
  (open-db (db db)))

(defmethod db-query ((db packy-db) query &key &allow-other-keys))
  
(defmethod db-get ((db packy-db) (key simple-string) &key &allow-other-keys)
  ;; lol
    (with-db (db (db db))
      (get-kv-str-raw db key)))

(defmethod close-db ((db packy-db) &key &allow-other-keys)
  (close-db (db db)))

(defmethod destroy-db ((db packy-db))
  (destroy-db (db db)))

(defmethod get-val ((obj packy-db) (elt simple-string) &optional data-type)
  (declare (ignore data-type))
  (db-get obj elt))

(defmethod get-db (dbs (name (eql :packy))))
