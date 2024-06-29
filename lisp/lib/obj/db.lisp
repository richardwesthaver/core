;;; lib/obj/db/proto.lisp --- Database Protocol

;;

;;; Commentary:

;; This set of 

;;; Code:
(in-package :obj/db)

;;; Vars
(declaim (sb-kernel:type-specifier *default-database-type* *default-database-collection-type*))
(defparameter *default-database-type* 'vector)
(defparameter *default-database-collection-type* 'list)

;;; Database
(defgeneric db (self)
  (:documentation "Return the Database associated with SELF."))

(defclass database ()
  ((db :initarg :db :accessor db)))

(defclass database-collection () ())

;; TODO 2024-05-30: maybe make into a macro?
(defgeneric make-db (engine &rest initargs &key &allow-other-keys)
  (:documentation "Dispatch initializer for databases. An ENGINE must be supplied, which is
usually a key such as :ROCKSDB or :SQLITE."))

(defgeneric connect-db (db &key &allow-other-keys)
  (:documentation "Connect the database DB."))

(defgeneric query-db (db query &key &allow-other-keys)
  (:documentation "Execute QUERY against DB."))

(defgeneric db-get (db key &key &allow-other-keys)
  (:documentation "Return the value associated with KEY from DB."))

(defgeneric (setf db-get) (db key val &key &allow-other-keys))

(defgeneric close-db (db &key &allow-other-keys)
  (:documentation "Close the database DB."))

(defgeneric open-db (self))

(defgeneric destroy-db (self)
  (:documentation "Destroy all traces of a database, deleting any on-disk data and shutting down
in-memory objects."))

(defgeneric find-db (dbs name &key &allow-other-keys)
  (:documentation "Return the db by NAME, from a collection of databases DBS."))

(defgeneric insert-db (dbs name &key &allow-other-keys)
  (:documentation "Inserts a database by NAME into the database-collection DBS."))

;;; Common
(defun slot-val (instance slot-name)
  (if (and instance
           (slot-boundp instance slot-name))
      (slot-value instance slot-name)))

(defgeneric get-val (object element &optional data-type)
  (:documentation "Returns the value in a object based on the supplied element name and possible
type hints.")
  (:method (object element &optional data-type)
  (when object
    (typecase (or data-type object)
      (hash-table
       (gethash element object))
      (standard-object
       (slot-val object element))
      (t
       (if data-type
           (cond 
             ((equal 'alist data-type)
              (second (assoc element object :test #'equal)))
             ((equal 'plist data-type)
              (get object element))
             (t
              (error "Does not handle this type of object. Implement your own get-val method.")))
           (if (listp object)
               (second (assoc element object :test #'equal))
               (error "Does not handle this type of object. Implement your own get-val method."))))))))
    

(defgeneric (setf get-val) (new-value object element &optional data-type)
  (:documentation "Set the value in a object based on the supplied element name and possible type
hints.")
  (:method (new-value object element &optional data-type)
    (typecase (or data-type object)
      (hash-table (setf (gethash element object) new-value))
      (standard-object (setf (slot-value object element) new-value))
      (t
       (if data-type
           (cond ((equal 'alist data-type)
                  (replace object (list (list element new-value))))
                 ((equal 'plist data-type)
                  ;;TODO: Implement this properly.
                  (get object element ))
                 (t
                  (error "Does not handle this type of object. Implement your own get-val method.")))
           (if (listp object)
               (replace object (list (list element new-value)))
               (error "Does not handle this type of object. Implement your own get-val method.")))))))
