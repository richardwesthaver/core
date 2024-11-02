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

(defclass schema ()
  ((fields :initarg :fields :accessor fields))
  (:documentation "Base class for all schema objects."))

(defun make-schema (&rest fields)
  (make-instance 'schema :fields (coerce fields 'vector)))

(defstruct field
  (name (symbol-name (gensym "#")) :type simple-string)
  (type t :type (or symbol list)))

(defmethod make-load-form ((self field) &optional env)
  (declare (ignore env))
  `(make-field :name ,(field-name self) :type ,(field-type self)))

(deftype field-vector () '(vector field))

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
  ((name :accessor name :initarg :name))
  (:documentation "Base class for simple schemas."))

(defun make-simple-schema (name &rest fields)
  (make-instance 'simple-schema :name name :fields (coerce fields 'field-vector)))

;;; Object Schema
(defclass object-schema (schema)
  ((classname :initarg :classname :accessor classname)))

(defmethod print-object ((schema object-schema) stream)
  (print-unreadable-object (schema stream :type t) (format stream "~A" (classname schema))))

(defstruct slot-field type name args)

(defun slot-field-eq (rec1 rec2)
  (and (eq (slot-field-name rec1) (slot-field-name rec2))
       (eq (slot-field-type rec1) (slot-field-type rec2))
       (indexed-slot-field-eq rec1 rec2)))

(defun indexed-slot-field-eq (rec1 rec2)
  (or (not (eq (slot-field-type rec1) :indexed))
      (eq (getf (slot-field-args rec1) :base)
          (getf (slot-field-args rec2) :base))))

(defun get-slot-fields-by-type (type schema)
  (remove-if-not (lambda (rec)
                   (eq (slot-field-type rec) type))
                 (fields schema)))

(defun class-instance-schema (class-obj)
  "Compute a schema representation from an instance of stored-class."
  (make-instance 'object-schema
                 :name (class-name class-obj)
                 :fields (compute-slot-fields class-obj)))

(defun compute-transient-schema (class-obj)
  (make-instance 'object-schema
                 :name (class-name class-obj)
                 :fields (append (compute-slot-fields class-obj)
                                 (compute-transient-slot-fields class-obj))))

(defparameter *slot-def-type-tags*
  '((:stored stored::stored-effective-slot-definition)
    (:indexed stored::indexed-effective-slot-definition)
    (:derived stored::derived-index-effective-slot-definition)
    (:cached stored::cached-effective-slot-definition)))

(defun compute-slot-fields (class-obj &optional (slot-tag-map *slot-def-type-tags*))
  "For each slot, compute a serializable record of the important info 
   in that slot"
  (mapcan (lambda (tagrec)
            (destructuring-bind (type slot-def-type) tagrec
              (compute-slot-fields-by-type type slot-def-type class-obj)))
          slot-tag-map))

(defun compute-transient-slot-fields (class-obj)
  (compute-slot-fields class-obj '((:transient transient-effective-slot-definition))))

(defmethod compute-slot-fields-by-type (type slot-def-type class-obj)
  "Default slot computation.  Capture the name and type tag for the definition"
  (mapcar (lambda (slotname)
            (make-slot-field :type type :name slotname :args nil))
          (find-slot-def-names-by-type class-obj slot-def-type nil)))

(defmethod compute-slot-fields-by-type ((type (eql :indexed)) slot-def-type class-obj)
  "Special handling for hierarchical indexing, capture the base class name of the index"
  (mapcar (lambda (slot-def)
            (make-slot-field :type type :name (slot-definition-name slot-def) 
                           :args `(:base ,(indexed-slot-base slot-def))))
          (find-slot-defs-by-type class-obj slot-def-type nil)))

;;; Debugging
(defgeneric dump-schema (self &optional stream)
  (:method ((self simple-schema) &optional (stream t))
    (format stream "Schema for ~A (~A)~%" (name self) self)
    (format stream "id: ~A~%" (id self))
    (call-next-method))
  (:method :after ((self object-schema) &optional (stream t))
    (dump-slots self stream)))

(defun dump-slots (schema &optional (stream t))
  (loop for rec in (fields schema) do
       (format stream "  ~A ~A ~A~%" (slot-field-name rec) (slot-field-type rec) (slot-field-args rec))))

;;; Sorting
(defmethod match-schemas ((sch1 null) sch2)
  nil)

(defmethod match-schemas (sch1 (sch2 null))
  nil)

(defmethod match-schemas ((sch1 simple-schema) (sch2 simple-schema))
  "Are the two schemas functionally equivalent?"
  (and (equal (name sch1) (name sch2))
       (equal (merge 'list 
                     (sorted-slots :stored sch1)
                     (sorted-slots :cached sch1)
                     #'symbol<)
              (merge 'list
                     (sorted-slots :stored sch2)
                     (sorted-slots :cached sch2)
                     #'symbol<))
       (equal (sorted-slots :indexed sch1)
              (sorted-slots :indexed sch2))
       (equal (sorted-slots :derived sch1)
              (sorted-slots :derived sch2))))

(defun symbol< (sym1 sym2) 
  (string< (symbol-name sym1) (symbol-name sym2)))

(defun sorted-slots (type schema)
  (let ((list (mapcar #'slot-field-name (get-slot-fields-by-type type schema))))
    (sort list #'symbol<)))

;;; Diffs
(defmethod schema-diff (new old)
  "Returns a list of lists :add, :rem, :change with one or two slot-fields"
  (let ((new-recs (fields new))
        (old-recs (fields old)))
    (labels ((find-old-rec (new-rec) 
               (find (slot-field-name new-rec) old-recs :key #'slot-field-name))
             (diff-add-change () 
               (loop for new-rec in new-recs collect
                    (aif (find-old-rec new-rec)
                         (unless (slot-field-eq new-rec it)
                           `(:change ,it ,new-rec))
                         `(:add ,new-rec))))
             (diff-rem () 
               (mapcar #'(lambda (rec) `(:rem ,rec))
                       (set-difference old-recs new-recs :key #'slot-field-name))))
      (remove-if #'null (append (diff-add-change) (diff-rem))))))


(defun diff-type (diff-entry) (car diff-entry))
(defun diff-recs (diff-entry) (cdr diff-entry))

(defun slot-defs-from-schema (schema args)
  "Need to handle default-initargs and other options to defclass"
  (destructuring-bind (&key (accessor-template #'default-template)
                            accessor-override &allow-other-keys) args
    (loop for rec in (fields schema) do
          (list :name (slot-field-name rec)
                :readers (compute-reader (classname schema) (slot-field-name rec)
                                         accessor-override accessor-template)
                :writers (compute-writer (slot-field-name rec)
                                         (classname schema) 
                                         accessor-override accessor-template )))))

(defun compute-reader (classname name override-fn template-fn)
  (or (and override-fn
           (funcall override-fn classname name :reader))
      (funcall template-fn classname name :reader)))

(defun compute-writer (classname name override-fn template-fn)	
  (or (and override-fn
           (funcall override-fn classname name :writer))
      (funcall template-fn classname name :writer)))

(defun default-template (classname name type)
  (ecase type
    (:reader (list (intern (format nil "~A-~A" classname name))))
    (:writer `((setf ,(intern (format nil "~A-~A" classname name)))))))

(defmethod default-class-constructor ((schema object-schema) &rest args
                                      &key superclasses &allow-other-keys)
  "Given a schema, construct a class overriding information as necessary
   :subclasses - a list of subclasses for this schema"
  (let ((name (name schema)))
    (ensure-class-using-class (find-class name :errorp nil) name
                :direct-superclasses superclasses
                :direct-slots (slot-defs-from-schema schema args)
                :metaclass 'stored-class)))

;;; Macros

(defun list-to-fields (fields)
  "List is assumed to be a list of lists where each element is of the form:
(name type &keys)"
  (make-array (length fields)
              :element-type 'field
              :initial-contents
              (loop for f in fields
                    collect (make-field :name (string-downcase (pop f)) :type (pop f)))))

(defmacro defschema (name super fields &rest options)
  "Define a new schema. DIRECT-SUPERCLASSES is the base SCHEMA class to inherit
from defaulting to SIMPLE-SCHEMA. FIELDS
are a list of field forms passed through LIST-TO-FIELDS and initialized in the
appropriate slot of the new class given by NAME. OPTIONS are the same as
DEFCLASS."
  (unless super
    (setf super '(simple-schema)))
  (let ((api
          (cond 
            ((member 'object-schema super)
             `((defun ,(symbolicate 'make- name) (name &rest fields)
                 (make-instance ',name :name name :fields (coerce fields 'vector)))))
            ((member 'simple-schema super)
             `((defun ,(symbolicate 'make- name) (name &rest fields)
                 (make-instance ',name :name name :fields (coerce fields 'vector))))))))
    `(prog1`
         (defclass ,name ,super ()
           (:default-initargs :fields (list-to-fields ',fields))
           ,@options)
       ,@api)))
