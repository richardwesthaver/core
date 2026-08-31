;;; schema.lisp --- RDB Schema Implementation

;; 

;;; Code:
(in-package :rdb)

(defschema rdb-schema (schema)
  ((:default (octet-vector . octet-vector)))
  (:documentation "A schema which may be read from a simple s-expression and ingested by an
RDB instance via LOAD-SCHEMA."))

(defschema rdb-object-schema (object-schema) ()
  (:documentation "An object schema which may be ingested by a RDB-STORE."))

;; Note that we don't use SIMPLE-CONS-COLUMN here because the NAME slot of
;; columns is stored in the underlying CF handle.
(defclass rdb-column (cons-column) ()
  (:default-initargs 
   :type (cons 'octet-vector 'octet-vector)))

(defun field-from-cf (cf)
  (make-field :name (%cf-name cf)
              :type (cons 'octet-vector 'octet-vector)))

(defclass rdb-data-source (data-source)
  ((db :type rdb :initarg :db :accessor db)
   (schema :type rdb-schema :initarg :schema :accessor schema)))

(defmethod initialize-instance :after ((self rdb-data-source) &key)
  (unless (or (slot-boundp self 'schema) (not (slot-boundp self 'db)))
    (setf (schema self) (schema (db self)))))

(defclass simple-column-family (column-family rdb-column) ()
  (:default-initargs :name (symbol-name (gensym "CF#")))
  (:documentation "COLUMN support for RocksDB Column Families."))

(defmethod load-schema ((self rdb) (schema schema))
  "Load SCHEMA into rdb database object SELF. This will add any missing CFs
and update existing key/value types for cfs with the same name. Existing CFs
only get their type slots updated on non-nil values."
  (loop for field across (fields schema)
        do (if-let ((col (find-column (string (name field)) self)))
             (load-field col field)
              (make-column self :class 'simple-column-family
                                :type (field-type field)
                                :name (string (name field))))
        finally (return self)))

(defun schema-from-simple-column-families (columns)
  "Convert a sequence of SIMPLE-COLUMN-FAMILYs to a SCHEMA."
  (apply 'make-schema 
         (map 'list 
              (lambda (x)
                (make-field :name (keywordicate (name x)) :type (column-type x)))
              columns)))

(defmethod load-field ((self simple-column-family) (field field))
  (let ((type (field-type field))
        (ctype (column-type self)))
    (typecase type
      (null nil)
      (atom (if (atom ctype) 
                (setf ctype (cons ctype type))
                (setf (cdr ctype) type)))
      (list (setf (car ctype) (car type)
                  (cdr ctype)
                  (if (and (listp (cdr type))
                           (= 1 (length (cdr type))))
                      (cadr type)
                      (cdr type)))))
    self))

(defmethod load-field ((self column-family) (field field))
  (let ((ty (field-type field))
        (ret (change-class self 'simple-column-family)))
    (with-slots (type) ret
      (typecase ty
        (null nil)
        (atom (if (atom type)
                  (setf type (cons type ty))
                  (setf (cdr type) ty)))
        (list (setf (car type) (car ty)
                    (cdr type)
                    (if (and (listp (cdr ty))
                             (= 1 (length (cdr ty))))
                        (cadr ty)
                        (cdr ty)))))
      ret)))

(defmethod change-class ((self field) (new-class (eql 'simple-column-family)) &key)
  (make-instance new-class :name (name self) :type (field-type self)))

(defmethod change-class ((self system-area-pointer) (new-class (eql 'simple-column-family)) &key)
  (let ((cf (sap-alien self (* rocksdb-column-family-handle))))
    (make-instance new-class :db cf :name (%cf-name cf))))

(defmethod change-class ((self column) (new-class (eql 'simple-column-family)) &key name)
  (let ((ret (make-instance new-class :type (column-type self))))
    (when name (setf (name ret) name))
    ret))
