;;; db.lisp --- Packy Database

;; 

;;; Code:
(in-package :skel/packy)

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

(defclass packy-db (rdb) ()
  (:default-initargs 
   :db (make-db :rocksdb 
                :name (namestring (merge-pathnames "db/" *packy-home*))
                :opts (default-rocksdb-options)
                :logger (rdb-log-default 10))))

(defmethod make-db ((engine (eql :packy)) &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'packy-db initargs))

(defun init-packy-db ()
  (setq *packy-db* (load-schema (make-db :packy) (make-instance 'packy-schema))))

(defun insert-pack (pack)
  (declare (ignore pack))
  (make-instance 'pack))
