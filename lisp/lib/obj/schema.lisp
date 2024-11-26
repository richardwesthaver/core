;;; schema.lisp --- Generic Schemas

;; Object Schema Machinery

;;; Code:
(in-package :obj/schema)

;;; Types
(eval-always
  (defvar *literal-value-types* '(boolean integer fixnum signed-byte unsigned-byte float double-float string)))

(deftype literal-value-type () `(member ,*literal-value-types*))

(deftype field-vector () '(vector field))

;;; Vars
(defvar *schema* nil)

;;; Generics
(defgeneric field (self n))
(defgeneric fields (self))
(defgeneric schema (self))
(defgeneric derive-schema (self))
(defgeneric apply-schema (schema object))
(defgeneric load-schema (self schema))
(defgeneric load-field (self field))

;; convenience interface for FIELD-VECTOR
(defclass column-vector () ((data :type simple-vector :accessor column-data)))

(defclass literal-value-vector (column-vector)
  ((type :type literal-value-type :initarg :type :accessor column-type)
   (data :initarg :data :accessor column-data)
   (size :type fixnum :initarg :size :accessor column-size)))

(defgeneric row-count (self)
  (:method ((self record-batch))
    (sequence:length (aref (column-data (record-batch-fields self)) 0))))

(defgeneric column-count (self)
  (:method ((self record-batch))
    (length (record-batch-fields self))))

(defgeneric column-literal-value (self)
  (:method ((self literal-value-vector))
    (column-data self)))

(defgeneric column-type (self)
  (:method ((self column-vector))
    (array-element-type (column-data self))))

(defgeneric column-value (self i)
  (:method ((self column-vector) (i fixnum))
    (aref (column-data self) i))
  (:method ((self literal-value-vector) (i fixnum))
    (if (or (< i 0) (>= i (column-size self)))
        (error 'simple-error :format-control "index out of bounds: ~A" :format-arguments i)
        (column-literal-value self))))

(defgeneric column-size (self)
  (:method ((self column-vector))
    (length (column-data self))))

(defgeneric scan-data (self projection)
  (:documentation "Scan the data source, selecting the specified columns."))

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

(defun make-fields (&rest fields)
  "Coerce a plist of the form :NAME TYPE into a FIELD-VECTOR."
  (let ((ret))
      (sb-int:doplist (k v) fields
        (push (make-field :name (string-downcase k) :type v) ret))
    (coerce (nreverse ret) 'field-vector)))

(defmethod print-object ((self schema) stream)
  (print-unreadable-object (self stream :type t)
    (format stream ":fields ~A" (map 'list 'field-name (fields self)))))

(defmethod make-load-form ((self schema) &optional env)
  (declare (ignore env))
  `(make-instance ,(class-of self) :fields ,(fields self)))

;;; Record Batch
(defstruct record-batch
  (schema (make-simple-schema (gensym "RECORD")) :type schema)
  (fields #() :type column-vector))

(defmethod schema ((self record-batch))
  (record-batch-schema self))

(defmethod make-load-form ((self record-batch) &optional env)
  (declare (ignore env))
  `(make-record-batch :schema ,(record-batch-schema self) :fields ,(record-batch-fields self)))

(defmethod field ((self record-batch) (n fixnum))
  (aref (column-data (record-batch-fields self)) n))

(defmethod fields ((self record-batch))
  (record-batch-fields self))

(defmethod schema ((self record-batch))
  (record-batch-schema self))

;;; Data Source
(defclass data-source ()
  ((schema :type schema :accessor schema)))

(defclass file-data-source (data-source)
  ((path :initarg :path :accessor path)))

;;; Schema Metadata
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

(defmethod id ((self simple-schema)) (name self))
(defmethod (setf id) (new (self simple-schema)) (setf (name self) new))

;;; Dynamic Schema
(defclass dynamic-schema (schema id) 
  ((fields :initarg :fields :accessor fields :dynamic t))
  (:metaclass dynamic-class))

;;; Object Schema
(defclass object-schema (schema)
  ((class-name :initarg :class-name :accessor schema-class-name)
   (successor :accessor schema-successor :initarg :successor :initform nil)
   (predecessor :accessor schema-predecessor :initarg :predecessor :initform nil))
  (:documentation "Keep a doubly linked list of schemas in the db"))

(defmethod print-object ((schema object-schema) stream)
  (print-unreadable-object (schema stream :type t) (format stream "~A" (schema-class-name schema))))

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
                 :name (schema-class-name class-obj)
                 :fields (compute-slot-fields class-obj)))

(defun compute-transient-schema (class-obj)
  (make-instance 'object-schema
                 :name (schema-class-name class-obj)
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
  (loop for rec across (fields schema) do
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

(defmethod match-schemas ((sch1 object-schema) (sch2 object-schema))
  "Are the two schemas functionally equivalent?"
  (and (equal (schema-class-name sch1) (schema-class-name sch2))
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
                :readers (compute-reader (schema-class-name schema) (slot-field-name rec)
                                         accessor-override accessor-template)
                :writers (compute-writer (slot-field-name rec)
                                         (schema-class-name schema) 
                                         accessor-override accessor-template )))))

(defun compute-reader (class-name name override-fn template-fn)
  (or (and override-fn
           (funcall override-fn class-name name :reader))
      (funcall template-fn class-name name :reader)))

(defun compute-writer (class-name name override-fn template-fn)	
  (or (and override-fn
           (funcall override-fn class-name name :writer))
      (funcall template-fn class-name name :writer)))

(defun default-template (class-name name type)
  (ecase type
    (:reader (list (intern (format nil "~A-~A" class-name name))))
    (:writer `((setf ,(intern (format nil "~A-~A" class-name name)))))))

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
    `(prog1
         (defclass ,name ,super ()
           (:default-initargs :fields (list-to-fields ',fields))
           ,@options)
       ,@api)))
