;;; lib/obj/db/proto.lisp --- Database Protocol

;;

;;; Code:
(in-package :obj/db)

;;; Database
(defgeneric db (self)
  (:documentation "Return the Database associated with SELF."))

(defclass database ()
  ((db :initarg :db :accessor db)))

(defgeneric make-db (engine &rest initargs &key &allow-other-keys))

(defgeneric connect-db (db &key &allow-other-keys))

(defgeneric db-query (db query &key &allow-other-keys))

(defgeneric db-get (db key &key &allow-other-keys))

(defgeneric (setf db-get) (db key val &key &allow-other-keys))

(defgeneric close-db (db &key &allow-other-keys))

(defgeneric open-db (self))

(defgeneric destroy-db (self))

(defgeneric find-db (dbs name)
  (:documentation "Returns the db by name."))

(defgeneric insert-db (dbs name &key base-path load-from-file-p)
  (:documentation "Inserts a db to the dbs hashtable. A base-path can be
supplied here that is independatn of the dbs base-path so that a
database collection can be build that spans multiple disks etc."))

;;; Common
(defun slot-val (instance slot-name)
  (if (and instance
           (slot-boundp instance slot-name))
      (slot-value instance slot-name)))

(defgeneric get-val (object element &optional data-type)
  (:documentation "Returns the value in a object based on the supplied element name and possible
type hints."))

(defgeneric (setf get-val) (new-value object element &optional data-type)
  (:documentation "Set the value in a object based on the supplied element name and possible type
hints."))

(defmethod get-val (object element &optional data-type)
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
               (error "Does not handle this type of object. Implement your own get-val method.")))))))

(defmethod (setf get-val) (new-value object element &optional data-type)
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
             (error "Does not handle this type of object. Implement your own get-val method."))))))
