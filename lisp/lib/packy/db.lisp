(in-package :packy/core)

(defclass packy-database (database) ()
  (:default-initargs 
   :db (make-rdb :name "packy")))

(defmethod make-db ((engine (eql :packy)) &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'packy-database initargs))

(defmethod connect-db ((db packy-database) &key &allow-other-keys)
  (declare (ignorable initargs))
  (with-slots (db) db
      (open-db db)))

(defmethod query-db ((db packy-database) query &key &allow-other-keys))
  
