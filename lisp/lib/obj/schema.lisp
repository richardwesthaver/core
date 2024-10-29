;;; schema.lisp --- Generic Schemas

;; Object Schema Machinery

;;; Code:
(in-package :obj/schema)

(defvar *schema* nil)

(defgeneric field (self n))
(defgeneric fields (self))
(defgeneric schema (self))
(defgeneric derive-schema (self))
(defgeneric load-schema (self schema))
(defgeneric load-field (self field))

(defclass schema () ())

(defstruct field
  (name (symbol-name (gensym "#")) :type simple-string)
  (type t :type (or symbol list)))

(deftype field-vector () '(vector field))

(defclass schema () ())

(defmethod print-object ((self schema) stream)
  (print-unreadable-object (self stream :type t)
    (format stream ":fields ~A" (map 'list 'field-name (fields self)))))

(defmethod make-load-form ((self schema) &optional env)
  (declare (ignore env))
  `(make-instance ,(class-of self) :fields ,(fields self)))

(defclass schema-metadata ()
  ((metadata :initarg :metadata :accessor schema-metadata)))

(defmethod make-load-form ((self schema-metadata) &optional env)
  (declare (ignore env))
  `(make-instance ,(class-of self) :metadata ,(schema-metadata self)))

;;; Simple Schema
(defclass simple-schema (schema) 
  ((fields :initarg :fields :accessor fields)))

(defun make-simple-schema (&rest fields)
  (make-instance 'simple-schema :fields (coerce fields 'field-vector)))

;;; Object Schema
(defclass object-schema (schema)
  ((name :accessor name :initarg :name :initform nil)
   (slot-fields :accessor fields :initarg :fields :initform nil)))

(defmethod print-object ((schema object-schema) stream)
  (print-unreadable-object (schema stream :type t) (format stream "~A" (name schema))))

(defstruct slot-rec type name args)

(defun slot-rec-eq (rec1 rec2)
  (and (eq (slot-rec-name rec1) (slot-rec-name rec2))
       (eq (slot-rec-type rec1) (slot-rec-type rec2))
       (indexed-slot-rec-eq rec1 rec2)))

(defun indexed-slot-rec-eq (rec1 rec2)
  (or (not (eq (slot-rec-type rec1) :indexed))
      (eq (getf (slot-rec-args rec1) :base)
          (getf (slot-rec-args rec2) :base))))

(defun get-slot-recs-by-type (type schema)
  (remove-if-not (lambda (rec)
                   (eq (slot-rec-type rec) type))
                 (fields schema)))

(defun class-instance-schema (class-obj)
  "Compute a schema representation from an instance of persistent-metaclass"
  (make-instance 'schema
                 :name (class-name class-obj)
                 :fields (compute-slot-recs class-obj)))

(defun compute-transient-schema (class-obj)
  (make-instance 'schema
                 :name (class-name class-obj)
                 :fields (append (compute-slot-recs class-obj)
                                    (compute-transient-slot-recs class-obj))))

(defparameter *slot-def-type-tags*
  '((:stored stored::stored-effective-slot-definition)
    (:indexed stored::indexed-effective-slot-definition)
    (:derived stored::derived-index-effective-slot-definition)
    (:cached stored::cached-effective-slot-definition)))

(defun compute-slot-recs (class-obj &optional (slot-tag-map *slot-def-type-tags*))
  "For each slot, compute a serializable record of the important info 
   in that slot"
  (mapcan (lambda (tagrec)
            (destructuring-bind (type slot-def-type) tagrec
              (compute-slot-recs-by-type type slot-def-type class-obj)))
          slot-tag-map))

(defun compute-transient-slot-recs (class-obj)
  (compute-slot-recs class-obj '((:transient transient-effective-slot-definition))))

(defmethod compute-slot-recs-by-type (type slot-def-type class-obj)
  "Default slot computation.  Capture the name and type tag for the definition"
  (mapcar (lambda (slotname)
            (make-slot-rec :type type :name slotname :args nil))
          (meta:find-slot-def-names-by-type class-obj slot-def-type nil)))

(defmethod compute-slot-recs-by-type ((type (eql :indexed)) slot-def-type class-obj)
  "Special handling for hierarchical indexing, capture the base class name of the index"
  (mapcar (lambda (slot-def)
            (make-slot-rec :type type :name (sb-mop:slot-definition-name slot-def) 
                           :args `(:base ,(stored::indexed-slot-base slot-def))))
          (meta:find-slot-defs-by-type class-obj slot-def-type nil)))
