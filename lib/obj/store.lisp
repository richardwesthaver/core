;;; store.lisp --- Data Store Protocols

;; Support for Lisp Stores.

;;; Commentary:

;; Based on work from Elephant and XDB.

;; A STORE plays a similar role to ORMs in blub languages, but better since we
;; have CLOS and MOP for ultimate control. The purpose of a STORE is to
;; orchestrate the persistence of objects of a specific meta-class called
;; STORED. The metaclass adds an additional allocation target :DATABASE for
;; slot-objects which indicates that any access to them from Lisp will be
;; delegated to the associated STORE.

;;; Code:
(in-package :obj/store)

;;; Variables
(defvar *lazy-db-instance-upgrading* nil
  "Only upgrade instances when loaded. This may require a chain of
transformations and delay reclamation of space, but it amortizes upgrade costs
over time. Not compatible with valid, up-to-date indices!")

(defvar *lazy-memory-instance-upgrading* nil
  "Walk through a given store's memory cache on class redefinition by default.
Setting this variable inhibits calling update-instance-for-redefined-class for
any instances that have been invalidated by the MOP.")

(defparameter *warn-when-dropping-stored-slots* t
  "Signal a continue-able error when the user is about to delete
a bunch of stored slot values on class redefinition. This is nil by default to
stop annoying message and confusing new users, but it will help keep users
from shooting themselves in the foot and losing significant amounts of data
during debugging and development. It can be disabled if change-class is used a
bunch in the application rather than just DEFCLASS changes interactively.

Note that the new class definition will take place even if you abort the
continue-able error; only the removal of the slots in the database is
prevented. You can access them again if you redefine your class once more.")

(defvar *store-spec* nil)
(defvar *store-lock* (make-mutex :name "STORE"))

;;; Stored Set
;; default implementation of simple sets using btrees
(defclass pset (stored-collection) ()
  (:documentation "An unordered stored collection of unique elements according to serializer
equal comparison"))

(defgeneric insert-item (item pset)
  (:documentation "Insert a new item into the pset"))

(defgeneric remove-item (item pset)
  (:documentation "Remove specified item from pset"))

(defgeneric map-pset (fn pset)
  (:documentation "Map operator for psets"))

(defgeneric find-item (item pset &key key test)
  (:documentation "Find a an item in the pset using key and test"))

(defgeneric pset-list (pset)
  (:documentation "Convert items of pset into a list for processing"))

(defgeneric build-pset (sc)
  (:documentation "Construct an empty default pset or backend specific pset.
                   This is an internal function used by make-pset"))

(defgeneric drop-pset (pset)
  (:documentation "Release pset storage to database for reuse"))

(defsclass default-pset (pset)
  ((btree :accessor pset-btree :initarg :btree)))

(defmethod drop-instance ((pset pset))
  (drop-pset pset)
  (call-next-method))

;;; Slot Access
(defmethod slot-value-using-class ((class stored-class) (instance stored-object) (slot-def set-valued-slot-definition))
  "Ensure that there is a slot-set in the slot (lazy instantiation)"
  (handler-case
      (call-next-method)
    (unbound-slot ()
      (setf (slot-value-using-class class instance slot-def)
            (build-slot-set (get-store instance))))))

(defmethod (setf slot-value-using-class) 
    (new-value (class stored-class) (instance stored-object) (slot-def set-valued-slot-definition))
  "Setting a value adds it to the slot set"
  (if (or (null new-value)
          (subtypep (type-of new-value) 'slot-set))
      (progn
        (slot-makunbound-using-class class instance slot-def)
        (call-next-method))
      (insert-item new-value (slot-value-using-class class instance slot-def))))

(defmethod slot-makunbound-using-class ((class stored-class) (instance stored-object) (slot-def set-valued-slot-definition))
  "Make sure we reclaim the pset storage"
  (awhen (and (slot-boundp-using-class class instance slot-def)
              (slot-value-using-class class instance slot-def))
    (drop-slot-set it))
  (call-next-method))

;;  Slot set helpers
(defmacro set-list (object slotname)
  "Sugar for getting a list from a set slot"
  `(slot-set-list (slot-value ,object ,slotname)))

(defmacro set-insert (item object slotname)
  "Sugar for inserting items under #'equal from the set slot"
  `(insert-item ,item (slot-value ,object ,slotname)))

(defmacro set-remove (item object slotname)
  "Sugar for removing items via #'equal from the set slot"
  `(remove-item ,item (slot-value ,object ,slotname)))

;;  A generic slot set implementation
(defclass slot-set () ()
  (:documentation "A proxy object for a set stored in a slot."))

(defsclass stored-slot-set (slot-set stored-pset) ()
  (:documentation "A default slot-set implementation"))

(defgeneric build-slot-set (sc)
  (:documentation "Construct an empty default pset or backend specific pset.
                   This is an internal function used by make-pset"))

(defgeneric slot-set-list (slot-set)
  (:documentation "Convert items of pset into a list for processing")
  (:method ((set stored-slot-set))
    (pset-list set)))

(defgeneric map-slot-set (fn slot-set)
  (:documentation "Map operator for psets")
  (:method (fn (set stored-slot-set))
    (map-pset fn set)))

(defgeneric drop-slot-set (pset)
  (:documentation "Release pset storage to database for reuse")
  (:method ((set stored-slot-set))
    (drop-instance set)))

;;; Associations
(defmethod slot-value-using-class 
    ((class stored-class) (instance stored-object) (slot-def association-slot-definition))
  (if (eq (association-type slot-def) :ref)
      (call-next-method)
      (get-associated instance slot-def)))

(defmethod (setf slot-value-using-class) 
    (new-value (class stored-class) (instance stored-object) (slot-def association-slot-definition))
  (add-association instance (slot-definition-name slot-def) new-value)
  new-value)

(defmethod slot-boundp-using-class 
    ((class stored-class) (instance stored-object) (slot-def association-slot-definition))
  (when (eq (association-type slot-def) :ref)
    (call-next-method)))

(defmethod slot-makunbound-using-class 
    ((class stored-class) (instance stored-object) (slot-def association-slot-definition))
  (when (eq (association-type slot-def) :ref)
    (remove-association-end class instance slot-def nil)
    (call-next-method))) ;; remove storage

;; =========================
;; Handling reads
;; =========================
(defun type-check-association (instance slot-def other-instance)
  (when (null other-instance)
    (return-from type-check-association t))
  (unless (subtypep (type-of other-instance) (foreign-classname slot-def))
    (cerror "Ignore and return"
            "Value ~A written to association slot ~A of instance ~A 
             of class ~A must be a subtype of ~A"
            other-instance (foreign-slotname slot-def) instance
            (type-of instance) (foreign-classname slot-def))
    (return-from type-check-association nil))
  (unless (equal (spec instance) (spec other-instance))
    (cerror "Ignore and return"
            "Cannot association objects from different stores:
             ~A is in ~A and ~A is in ~A"
            instance (get-store instance)
            other-instance (get-store other-instance))
    (return-from type-check-association nil))
  t)

(defun get-associated (instance slot-def)
  (let* ((fclass (get-foreign-class slot-def))
         (fslot (get-foreign-slot fclass slot-def))
         (sc (get-store instance))
         (index (get-association-index fslot sc)))
    (flet ((map-obj (value oid)
             (declare (ignore value))
             (store-recreate-instance sc oid)))
      (declare (dynamic-extent (function map-obj)))
      (map-btree #'map-obj index :value (oid instance) :collect t))))

;; ==========================
;;  Handling updates
;; ==========================
(defun update-association-end (class instance slot-def target)
  "Get the association index and add the target object as a key that
   refers back to this instance so we can get the set of referrers to target"
  (let ((index (get-association-index slot-def (get-store instance))))
    (when (and (eq (association-type slot-def) :ref)
               (slot-boundp-using-class class instance slot-def))
      (remove-kv (oid (slot-value-using-class class instance slot-def)) (oid instance) index))
    (when (not (null instance))
      (setf (get-value (oid target) index) (oid instance)))))

(defun remove-association-end (class instance slot-def associated)
  (let ((index (get-association-index slot-def (get-store instance))))
    (if (and (eq (association-type slot-def) :ref)
             (slot-boundp-using-class class instance slot-def))
        (remove-kv (oid (slot-value-using-class class instance slot-def)) (oid instance) index)
        (when associated ;it is possible that the original association
                         ;slot was not bound at the time of
                         ;deletion. thus, remove the entry only when
                         ;it is bound
          (remove-kv (oid associated) (oid instance) index)))))

(defun update-other-association-end (class instance slot-def other-instance)
  "Update the association index for the other object so that it maps from
   us to it.  Also add error handling."
  (declare (ignore class))
  (let* ((fclass (class-of other-instance))
         (fslot (get-foreign-slot fclass slot-def))
         (sc (get-store other-instance)))
    (update-association-end fclass other-instance fslot instance)
    (when (eq (association-type slot-def) :ref)
      (stored-slot-writer sc instance other-instance (slot-definition-name fslot)))))

(defun get-foreign-class (slot-def)
  (find-class (foreign-classname slot-def)))

(defun get-foreign-slot (fclass slot-def)
  (find-slot-def-by-name fclass (foreign-slotname slot-def)))

;;  Late-binding Initialization
(defun get-association-index (slot-def sc)
  (ifret (get-association-slot-index slot-def sc)
    (aif (get-store-association-index slot-def sc)
         (progn (add-association-slot-index it slot-def sc) it)
         (let ((new-idx (make-dup-btree sc)))
           (add-slot-index sc new-idx (association-slot-base slot-def) (slot-definition-name slot-def))
           (add-association-slot-index new-idx slot-def sc)
           new-idx))))

(defun get-store-association-index (slot-def sc)
  (let* ((master (index-root sc))
         (base (association-slot-base slot-def))
         (slotname (slot-definition-name slot-def)))
    (get-value (cons base slotname) master)))

;;  Association-specific slot API
(defun add-association (instance slot associated)
  (let* ((sc (get-store instance))
         (class (class-of instance))
         (slot-def (if (symbolp slot) (find-slot-def-by-name class slot) slot)))
    (when (null slot-def)
      (error "Slot ~A not found in class ~A for instance ~A" slot class instance))
    (when (type-check-association instance slot-def associated)
      (ensure-transaction (:store sc)
        (case (association-type slot-def)
          (:ref (update-association-end class instance slot-def associated)
                (stored-slot-writer sc associated instance (slot-definition-name slot-def)))
          (:m21 (update-other-association-end class instance slot-def associated))
          (:m2m (update-association-end class instance slot-def associated)
                (update-other-association-end class instance slot-def associated)))))))

(defun remove-association (instance slotname associated)
  (let* ((class (class-of instance))
         (fclass (class-of associated))
         (slot-def (if (symbolp slotname) (find-slot-def-by-name class slotname) slotname))
         (fslot (get-foreign-slot fclass slot-def))
         (sc (get-store associated)))
    (when (null slot-def)
      (error "Slot ~A not found in class ~A for instance ~A" slotname class instance))
    (when (type-check-association instance slot-def associated)
      (ensure-transaction (:store sc)
        (case (association-type slot-def)
          (:ref (when (slot-boundp-using-class class instance slot-def)
                  (slot-makunbound-using-class class instance slot-def)))
          (:m21 (when (slot-boundp-using-class fclass associated fslot)
                  (slot-makunbound-using-class fclass associated fslot)))
          (:m2m (remove-association-end fclass associated fslot instance)
                (remove-association-end class instance slot-def associated)))))))

(defun get-associations (instance slot)
  (slot-value instance (if (symbolp slot) slot (slot-definition-name slot))))

(defun associatedp (instance slot associated)
  (find associated (get-associations instance slot)))

;;; IDs
(defgeneric next-oid (store)
  (:documentation
   "The source of unique object IDs."))

(let ((%next-oid -1))
  (defmethod next-oid (store)
    (incf %next-oid)))

(defgeneric next-cid (store)
  (:documentation
   "The source of unique class schema IDs."))

(let ((%next-cid -1))
  (defmethod next-cid (store)
    (incf %next-cid)))

(defun unindex-slot-value (sc key value old-name old-base)
  (let* ((master (index-root sc))
         (index (get-value (cons old-base old-name) master)))
    (remove-kv key value index)))

;;; Schema
(defclass stored-object-schema (object-schema upgradable-schema) ())

(defmethod has-class-schema-p ((class stored-class))
  (when-let ((s (get-class-schema class)))
    (typep s 'stored-object-schema)))

(defmethod print-object ((schema stored-object-schema) stream)
  (print-unreadable-object (schema stream :type t)
    (format stream "~A ~A (s: ~A p: ~A)"
            (id schema) (schema-class-name schema)
            (schema-successor schema) (schema-predecessor schema))))

(defun make-stored-object-schema (cid class-schema)
  (let ((schema (logical-copy-schema 'stored-object-schema class-schema)))
    (setf (id schema) cid)
    schema))

(defun logical-copy-schema (type schema)
  (assert (subtypep type 'schema:schema))
  (make-instance type
    :class-name (schema-class-name schema)
    :fields (copy-list (fields schema))))

(defun copy-schema (type schema)
  (assert (subtypep type 'schema))
  (let ((new 
         (make-instance type
                        :name (schema:schema-class-name schema)
                        :successor (schema:schema-successor schema)
                        :predecessor (schema:schema-predecessor schema)
                        :fields (copy-array (schema:fields schema)))))
    (when (subtypep (type-of schema) 'upgradable-schema)
      (setf (id new) (id schema))
      (setf (upgrade new) (upgrade schema))
      (setf (version new) (version schema)))
    new))

;;; Classes
(defgeneric recreate-instance (instance &rest initargs &key &allow-other-keys)
  (:method ((instance t) &rest args)
    (declare (ignore args))
    instance)
  (:method ((instance stored-object) &rest args &key oid schema (st *store*))
    (declare (ignore args))
    ;; Initialize basic instance data
    (initial-stored-setup instance :oid oid :store st)
  ;; Update db instance data
  (when schema
    (let ((official-schema (lookup-schema st (class-of instance))))
      (unless (eq (name schema) (name official-schema))
        (upgrade-db-instance instance official-schema schema nil))))
  ;; Load cached slots, set, assoc values, etc.
  (shared-initialize instance t :oid oid)
  instance)
  (:method ((instance stored-collection) &rest initargs &key oid (st *store*))
    (declare (ignore initargs))
    ;; Initialize basic instance data
    (initial-stored-setup instance :oid oid :store st)
    ;; Load cached slots, set, assoc values, etc.
    (shared-initialize instance t :oid oid)
    instance))

(defmethod recreate-instance-using-class ((class t) &rest initargs &key &allow-other-keys)
  "Implement a subset of the make-instance functionality to avoid initialize-instance
   calls after the initial creation time"
  (apply #'recreate-instance (allocate-instance class) initargs))

;; Class Redefinition
(defmethod update-instance-for-redefined-class :around ((instance stored-object) added-slots discarded-slots property-list &rest initargs)
  (declare (ignore discarded-slots added-slots initargs))
  (let* ((st (get-store instance))
;;	 (class (class-of instance))
         (current-schema (get-current-db-schema st (type-of instance))))
;;    (unless (match-schemas (%class-schema class) current-schema))
      (prog1 
          (call-next-method)
        (let ((prior-schema (aif (schema:schema-predecessor current-schema)
                                 (get-store-schema st it)
                                 (error "If the schemas mismatch, a derived store schema should have been computed"))))
          (assert (and current-schema prior-schema))
          (upgrade-db-instance instance current-schema prior-schema property-list)))))

(defmethod update-instance-for-different-class :after ((previous stored-object) (current stored-object) 
                                                        &rest initargs &key)
  ;; Update db to new class configuration
  ;; - handle indices, removals, associations and additions
  (let* ((sc (get-store current))
         (current-schema (lookup-schema sc (class-of current)))
         (previous-schema (lookup-schema sc (class-of previous))))
    (assert (eq sc (get-store previous)))
    (change-db-instance current previous current-schema previous-schema)
    ;; Deal with new stored slot, cached and transient initialization
    (let* ((diff-entries (schema-diff current-schema previous-schema))
           (add-entries (remove-if-not (lambda (entry) (eq :add (diff-type entry))) diff-entries))
           (add-names (when add-entries (mapcar #'name (mapcan #'diff-recs add-entries)))))
      (apply #'shared-initialize current add-names initargs))))

;;; Store
(defclass store () 
  ((spec :type list
         :accessor spec
         :initform nil
         :initarg :spec
         :documentation "Data store initialization functions are
         expected to initialize :spec on the call to make-instance")
   ;; Generic support for the object, indexing and root protocols
   (root 
    :reader store-root 
    :documentation "This is an instance of the data store btree. It should have an OID that is
fixed in the code and does not change between sessions. Usually this is
something like 0, 1 or -1")
   (schema-table 
    :reader schema-table
    :documentation "Schema id to schema database table")
   (schema-name-index
    :reader schema-name-index
    :documentation "Schema name to schema database table")
   (schema-cache 
    :accessor schema-cache :initform (make-cache-table :test 'eq)
    :documentation "This is a cache of class schemas stored in the database indexed by CID.")
   (schema-classes 
    :accessor schema-classes :initform nil
    :documentation "A list of all classes that have a cached schema value so we can shutdown
cleanly.")
   (schema-cache-lock 
    :accessor schema-cache-lock :initform (make-mutex :name "cache-lock")
    :documentation "Protection for updates to the cache from multiple threads. Do not override.")
   ;; Instance storage
   (instance-index
    :reader instance-index
    :documentation "OID->CID table.")
   (class-index 
    :reader class-index
    :documentation "CID->OID table (reverse map).")
   (instance-cache 
    :accessor instance-cache :initform (make-cache-table :test 'eql)
    :documentation 
    "Part of the meta-class protocol - data stores should not override the default
behavior.")
   (instance-cache-lock 
    :accessor instance-cache-lock :initform (make-mutex :name "instance-cache")
    :documentation "Protection for updates to the cache from multiple threads. Do not override.")
   ;; Root table for all indices
   (index-root
    :reader index-root
    :documentation 
    "This is another root for class indexing that is also a data store specific
stored btree instance with a unique OID that persists between sessions. No
cache is needed because we cache in the class slots.")
   (serializer :accessor serializer :initform nil)
   (deserializer :accessor deserializer :initform nil))
  (:documentation "Base class for all STOREs. The role of a STORE is similar to an ORM in the
sense that it supports querying and modification of persistent CLOS objects
via database access. A STORE maintains a collection of tables and a btree. It
supports the STORED metaprotocol implemented by STORED-OBJECT instances. See
DEFSCLASS for the available class-specific options in the generic interface."))

(defmethod print-object ((self store) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (second (spec self)))))

(defmethod initialize-instance :before  ((instance stored)
                                         &rest initargs
                                         &key oid
                                              store)
  "Each stored instance has an oid and a home store spec"
  (declare (ignore initargs))
  (initial-stored-setup instance :oid oid :store store))

(defun initial-stored-setup (instance &key oid store)
  (if oid
      (setf (oid instance) oid)
      (register-new-instance instance (class-of instance) store))
  (setf (spec instance) (spec store))
  (cache-instance store instance))

(defun class-schema-id (st class)
  (if (subtypep (class-name class) 'btree)
      (default-class-id (class-name class) st)
      (id (lookup-schema st class))))

(defmethod register-instance ((self list) class instance)
  (set-instance-schema-id self (oid instance) (class-schema-id self class)))

(defmethod register-instance ((st store) cl instance)
  (set-instance-schema-id st (oid instance) (class-schema-id st cl)))

(defmethod set-instance-schema-id ((st store) oid cid)
  (let ((table (instance-index st)))
    (delete-key oid table)
    (setf (get-value oid table) cid)))

(define-condition missing-stored-instance (simple-condition)
   ((oid :initarg :oid :accessor error-oid)
    (spec :initarg :spec :accessor error-spec)))

(defun missing-stored-instance (oid spec)
  (cerror "Return a proxy object"
          'missing-stored-instance
          :format-control "Instance with OID ~A is not stored in ~A"
          :format-arguments (list oid spec)
          :oid oid
          :spec spec))

(defmethod get-instance-class ((st store) oid &optional classname)
  "Get the class object using the oid or using the provided classname"
  (when classname
    (return-from get-instance-class (find-class classname)))
  (let ((cid (oid->schema-id oid st)))
    (unless cid
      (missing-stored-instance oid (spec st))
      (return-from get-instance-class (find-class 'stored-object)))
    (get-schema-id-class st cid)))

(defmethod get-schema-id-class ((st store) cid)
  "Get the class given the schema id"
  (aif (default-class-id-type cid st)
       (find-class it)
       (let ((schema (get-store-schema st cid)))
         (values (find-class (schema-class-name schema)) schema))))

(defmethod store-recreate-instance ((st store) oid &optional classname)
  "Called by the deserializer to return an instance"
  (handler-case 
      (progn 
        ;; Quick test since only the GC deletes object references
        (awhen (get-cached-instance st oid)
          (return-from store-recreate-instance it))
        ;; Update cache unless someone has before us!
        (with-mutex ((instance-cache-lock st))
          (aif (get-cached-instance st oid) it
               (multiple-value-bind (class schema) (get-instance-class st oid classname)
                 (recreate-instance-using-class class :oid oid :store st :schema schema)))))
    (missing-stored-instance (e)
      (signal e))))

(defmethod get-slot-def-index ((def association-effective-slot-definition) sc)
  "Since endpoints of an association implement an index we should be able to perform
   inverted-index relation functions on them directly"
  (get-association-index def sc))

(defun register-new-instance (instance class store)
  (setf (oid instance) (next-oid store))
  (register-instance store class instance))

(defun check-valid-store (store)
  (if-let ((ok (subtypep (type-of store) 'store)))
    ok
    (error "This function requires a valid store")))

(defmethod build-pset ((sc store))
  "Default pset method; override if backend has better policy"
  (let ((btree (make-dup-btree sc)))
    (make-instance 'default-pset :btree btree :sc sc)))

(defun make-pset (&key items pset (store *store*))
  (let ((new-pset (build-pset store)))
    (when (and items pset)
      (error "Can only initialize a new pset with item list or pset to copy, not both"))
    (when items
      (mapc (lambda (item)
              (insert-item item new-pset))
            items))
    (when pset
      (map-pset (lambda (item)
                  (insert-item item new-pset))
                pset))
    new-pset))

(defmethod insert-item (item (pset default-pset))
  (setf (get-value item (pset-btree pset)) t)
  item)

(defmethod remove-item (item (pset default-pset))
  (delete-key (pset-btree pset) item)
  item)

(defmethod find-item (item (pset default-pset) &key key (test #'equal))
  (if (not (or key test))
      (get-value item (pset-btree pset))
      (map-btree (lambda (elt dc)
                   (declare (ignore dc))
                   (let ((cmpval (if key (funcall key elt) elt)))
                     (if (funcall test item cmpval)
                         (return-from find-item elt))))
                 (pset-btree pset))))

(defmethod map-pset (fn (pset default-pset))
  (map-btree (lambda (key value) 
               (declare (ignore value))
               (funcall fn key))
             (pset-btree pset))
  pset)

(defmethod pset-list ((pset default-pset))
  (map-btree #'(lambda (k v) 
                 (declare (ignore v))
                 k) 
             (pset-btree pset) :collect t))

(defmethod drop-pset ((pset default-pset))
  (ensure-transaction (:store *store*)
    (awhen (pset-btree pset)
      (drop-btree it))))

(defmethod build-slot-set ((sc store))
  (let ((btree (make-btree sc)))
    (make-instance 'stored-slot-set :btree btree :sc sc)))

(defmethod drop-instance ((inst stored-object))
  (drop-instance-slots inst)
  (call-next-method))

(defmethod drop-instance ((inst stored))
  (let ((sc (get-store inst)))
    (with-mutex ((instance-cache-lock sc))
      (remhash (oid inst) (instance-cache sc)))
    (delete-key (oid inst) (instance-index sc))))

(defun drop-instance-slots (instance)
  "A helper function for drop-instance, that deletes the storage of 
   stored slots of instance from the db"
  (let ((class (class-of instance)))
    (loop for slot-def in (class-slots class)
       when (stored-p slot-def)
       do (slot-makunbound-using-class class instance slot-def))))

(defun dropped-instance-p (st oid)
  "An instance has not been dropped if it is in the instance
   table and has a valid class id"
  (multiple-value-bind (cid found?)
      (get-value oid (instance-index st))
    (and cid found?)))

(defmethod oid->schema-id (oid (st store))
  (get-value oid (instance-index st)))

(defgeneric default-class-id (base-type sc)
  (:documentation "A method implemented by the store for providing
   fixed class ids for basic btree derivative types")
  (:method ((base-type t) (sc t))
    (sxhash base-type)))

(defgeneric default-class-id-type (id sc)
  (:documentation "A method implemented by the store which provides
   the type associated with a default id or nil if the id does not match"))

(defgeneric reserved-oid-p (sc oid)
  (:documentation "Is this OID reserved by the store? GC doesn't touch"))

(defmethod add-class-store-schema (st (class stored-class) schema)
  "NOTE: Needs to be lock protected."
  (pushnew (class-name class) (schema-classes st))
  (remove-class-store-schema st class)
  (setf (get-store-schemas class)
        (acons (spec st) schema (get-store-schemas class))))

(defmethod remove-class-store-schema (st (class stored-class))
  "NOTE: Needs to be lock protected."
  (setf (get-store-schemas class)
        (remove (spec st) (get-store-schemas class) 
                :key #'car :test #'equalp)))

(defmethod get-class-store-schema (st (class stored-class))
  (awhen (assoc (spec st) (get-store-schemas class))
    (cdr it)))

(defmethod lookup-schema ((st store) (class stored-class))
  "Get the latest db class schema from caches, etc."
  ;; Lookup class cached version
  (awhen (get-class-store-schema st class) 
    (when (eq (schema:schema-successor it) nil)
      (return-from lookup-schema it)))
  ;; Lookup stored version
  (aif (get-current-db-schema st (class-name class))
       ;; Store it
       (prog1 it
         (add-class-store-schema st class it))
       ;; Or create it
       (create-store-schema st class)))

(defmethod get-store-schema ((st store) schema-id &optional class)
  "Find the db class schema by schema id. CLASS needs to be supplied
  if the class object isn't registered via (SETF FIND-CLASS) yet."
  (assert (typep schema-id 'fixnum))
  ;; Lookup in store cache
  (std/macs:ifret (get-cache schema-id (schema-cache st))
         ;; Lookup in store table
         (let* ((schema (get-value schema-id (schema-table st)))
                (class (or class (find-class (schema:schema-class-name schema)))))
           (assert schema)
           ;; Update store cache
           (with-mutex ((schema-cache-lock st))
             (setf (get-cache schema-id (schema-cache st)) schema))
           ;; Also cache in class slot
           (add-class-store-schema st class schema)
           schema)))

(defmethod finalize-inheritance :after ((class stored-class))
  (ensure-schemas class))

(defmethod upgrade-class-slot ((sc store) class (diff-type (eql :add)) recs)
  "At the store level, we'll only need to deal with structures that are at the
   class level, not managed in the individual instance"
  (declare (ignore class))
  (with-slots (name type args) (first recs)
    (case type
      (:indexed (add-slot-index sc (make-dup-btree sc) (getf args :base) name))
      (:derived (add-slot-index sc (make-dup-btree sc) (getf args :base) name))
      (:association nil))))

(defmethod upgrade-class-slot ((sc store) class (diff-type (eql :rem)) recs)
  "Drop index and association storage on upgrade.  Loss of data for associations should
   be flagged during the redefinition."
  (declare (ignore class))
  (with-slots (name type args) (first recs)
    (case type
;;      (:indexed (drop-slot-index sc (getf args :base) name))
      (:association nil))))

(defmethod upgrade-class-slot ((sc store) class (diff-type (eql :change)) recs)
  "For now, we can effectively remove and add at the store level"
  (upgrade-class-slot sc class :rem (list (first recs)))
  (upgrade-class-slot sc class :add (list (second recs))))

(defun lookup-con-spec (spec)
  (cdr (or (assoc spec *store-spec*)
           (assoc spec *store-spec* :test #'equalp))))

(defmethod synchronize-stores-for-class (class)
  "Synchronize all stores connected to a given class.  Meant to be
   called during class redefinition to keep all DB instances in sync."
  (let ((class-schema (get-class-schema class)))
    (loop for (spec . db-schema) in (get-store-schemas class) do
     (unless (match-schemas class-schema db-schema)
       (let ((store (lookup-con-spec spec)))
         (synchronize-store-class store class class-schema db-schema)
         (unless *lazy-memory-instance-upgrading*
           (upgrade-all-memory-instances store))
         (unless *lazy-db-instance-upgrading*
           (upgrade-all-db-instances store class-schema)))))))

(defmethod synchronize-store-class ((sc store) class class-schema old-schema)
  "Synchronizing a store means adding/removing indices, upgrading
   the default schema if necessary, etc."
  (format t "~&Synchronizing ~A in ~A~%" (schema-class-name class-schema) (spec sc))
  (let* ((class (or class (find-class (schema-class-name class-schema))))
         (new-schema (create-store-schema sc class))
     (diff (schema-diff new-schema old-schema)))
    ;; Chain schemas
    (setf (schema-successor old-schema) (id new-schema))
    (setf (schema-predecessor new-schema) (id old-schema))
    (update-store-schema sc old-schema)
    (update-store-schema sc new-schema t)
    ;; Update the class
    (loop for entry in diff do
     (upgrade-class-slot sc class (diff-type entry) (diff-recs entry)))))

(defmethod ensure-schemas ((instance stored-class))
  "Constructs the metaclass schema when the class hierarchy is valid"
  (let* ((old-schema (get-class-schema instance))
     (new-schema (class-instance-schema instance)))
    (assert new-schema)
    ;; Stop synchronization if necessary to allow for reversing the
    ;; interactive re-definition
    (when (and old-schema *warn-when-dropping-stored-slots*)
      (warn-on-reinitialization-data-loss instance))
    ;; Update schema chain
    (setf (schema-predecessor new-schema) old-schema)
    (setf (get-class-schema instance) new-schema)
    (and *store* (not (subtypep (class-name instance) 'btree))
      (lookup-schema *store* instance)) ; ensure db schema of user-defined classes
    ;; Cleanup some slot values
    (let ((idx-state (get-class-indexing instance)))
      (when (consp idx-state)
    (setf (get-class-indexing instance) (first idx-state))))
    ;; Compute derived index triggers
    (awhen (stored::derived-index-slot-defs instance)
      (stored::compute-derived-index-triggers instance it))
    ;; Synchronize instances to new schemas
    (when (and old-schema (not (match-schemas new-schema old-schema)))
      (synchronize-stores-for-class instance))
    (and *store*
         (not (subtypep (class-name instance) 'btree))
         (not (match-schemas (lookup-schema *store* instance) new-schema))
         (synchronize-stores-for-class instance))
    instance))

(defmethod create-store-schema ((st store) class)
  "We don't have a cached store schema, so create a new one"
  (ensure-finalized class)
  (let ((schema (make-stored-object-schema (next-cid st) (get-class-schema class))))
    ;; Add to database
    (setf (get-value (id schema) (schema-table st))
          schema)
    ;; Let get-store-schema cache it for us
    (get-store-schema st (id schema) class)))

(defmethod update-store-schema ((st store) schema &optional update-cache)
  "Use this to update the schema version that is on store and in 
   all the various caches"
  (assert (typep schema 'stored-object-schema))
  (assert (id schema))
  (let ((schema-id (id schema)))
    (set-store-schema st schema-id schema)
    (when update-cache
      (with-mutex ((schema-cache-lock st))
        (setf (get-cache schema-id (schema-cache st)) schema))
      (awhen (find-class (schema:schema-class-name schema) nil)
        (add-class-store-schema st (find-class (schema:schema-class-name schema)) schema)))))


(defmethod set-store-schema ((st store) schema-id schema)
  "Insert a new schema into the store table"
  (setf (get-value schema-id (schema-table st))
        schema))

(defmethod remove-store-schema ((st store) schema-id)
  "Remove a schema from the store table; uncache separately"
  (delete-key schema-id (schema-table st)))

(defmethod uncache-store-schema ((st store) schema-id)
  (handler-case
      (progn
        (with-mutex ((schema-cache-lock st))
          (remhash schema-id (schema-cache st)))
        (remove-class-store-schema st (get-schema-id-class st schema-id)))
    (program-error (e) ;; in case the class is gone for some reason
      (warn "Error ~A in uncache-store-schema , ignoring" e)
      nil)))

(defun get-current-db-schema (sc name)
  (awhen (sort (get-db-schemas sc name)
               #'> :key #'id)
    (car it)))

(defun get-db-schemas (st classname)
  "Return schemas ordered oldest to youngest (ascending cids)"
  (sort
   (map-btree #'(lambda (cname schema)
                  (declare (ignore cname))
                  schema)
              (schema-name-index st)
              :value classname :collect t)
   #'<
   :key #'id))

(defun update-derived-slot (class instance derived-slot-def)
  "Make a copy of the functionality here to be more efficient"
  (let ((sc (get-store instance)))
    (multiple-value-bind (new-value index?)
        (funcall (derived-fn derived-slot-def) instance)
      (when index?
        (update-slot-index sc class instance derived-slot-def new-value)
        (stored-slot-writer sc new-value instance 
                                (slot-definition-name derived-slot-def))))))

(defun derived-index-updater (class instance written-slot-def)
  "Compute the derived indices to update from the slot-def that is
   being written to.  Should be called in a transaction"
  (awhen (derived-slot-triggers written-slot-def)
    (dolist (derived-slot-def it)
      (update-derived-slot class instance derived-slot-def))))

(defun update-slot-index (sc class instance slot-def new-value)
  "Update an index value when written"
  (let ((oid (oid instance)))
      (let* ((idx (get-slot-def-index slot-def sc))
             (old-value-bound-p (slot-boundp-using-class class instance slot-def))
             (old-value (when old-value-bound-p
                          (slot-value-using-class class instance slot-def))))
        (unless idx
          (setf idx (ensure-slot-def-index slot-def sc)))
        (when old-value-bound-p 
          (remove-kv old-value oid idx))
        (setf (get-value new-value idx) oid))))

(defun get-store-index (slot-def sc)
  "Get the slot-def's index from the store"
  (let* ((master (index-root sc))
         (base (indexed-slot-base slot-def))
         (name (slot-definition-name slot-def)))
    (get-value (cons base name) master)))

(defun ensure-slot-def-index (slot-def sc)
  "If a slot's index does not exist, create it"
  (aif (get-store-index slot-def sc)
       (progn (add-slot-def-index it slot-def sc) it)
       (let ((new-idx (make-btree sc)))
         (add-slot-index sc new-idx (indexed-slot-base slot-def) (slot-definition-name slot-def))
         (add-slot-def-index new-idx slot-def sc)
         new-idx)))

(defmethod add-slot-index ((sc store) new-index class-name index-name)
  "Add it to the index table and the class slot def"
  (setf (get-value (cons class-name index-name) (index-root sc))
        new-index))

(defmethod drop-slot-index ((sc store) class-name index-name)
  (clear-slot-def-index (find-slot-def-by-name (find-class class-name) index-name) sc)
  (delete-key (cons class-name index-name) (index-root sc)))

(defmethod rebuild-slot-index ((sc store) class-name index-name)
  (drop-slot-index sc class-name index-name)
  (let ((class (find-class class-name)))
    (ensure-slot-def-index (find-slot-def-by-name class index-name) sc)
    (map-class #'(lambda (instance)
                   (when (slot-boundp instance index-name)
                     (update-slot-index sc class instance
                                        (find-slot-def-by-name class index-name)
                                        (slot-value instance index-name))))
               class)))

(defun rebuild-slot-indices (sc class)
  "Rebuild all slot indices for CLASS, or all known classes
  if CLASS is NIL. CLASS may be a class or class name."
  (let* ((classes (list* (etypecase class
                            (null (known-classes sc))
                            (class class)
                            (symbol (find-class class)))))
         (class-names (mapcar #'class-name classes)))
    (loop for class in classes
          for class-name in class-names
          do (progn
               (format t "=== class ~S~%" class)
               (dolist (slotname (indexed-slot-names class))
                 (ensure-finalized class) ; for CLASS-SLOTS
                 (when (member slotname (class-slots class) :key #'slot-definition-name)
                   (format t "slot index ~S~%" slotname)
                   (rebuild-slot-index sc class-name slotname)))))))

(defun known-classes (sc)
  "Return all classes that are known both to SC and the current
  Lisp image."
  (remove-duplicates
    (remove nil
            (maphash (lambda (cid schema)
                         (declare (ignore cid))
                         (let ((class (find-class (name schema) nil)))
                           (unless class
                             (warn "Class ~S not defined, ignoring." (name schema)))
                           class))
                       (schema-table sc)))))

(defun map-class (fn class &key collect oids (sc *store*))
  "Perform a map operation over all instances of class.  Takes a
   function of one argument, a class instance."
  (flet ((map-fn (cidx pcidx oid)
           (declare (ignore cidx pcidx))
           (funcall fn (store-recreate-instance sc oid)))
         (map-oid-fn (cidx pcidx oid)
           (declare (ignore cidx pcidx))
           (funcall fn oid)))
    (declare (dynamic-extent (function map-fn) (function map-oid-fn)))
    (let* ((classobj (if (symbolp class) (find-class class) class))
           (classname (if (symbolp class) class (class-name class)))
           (db-schemas (get-db-schemas sc classname))
           (schema-ids (if db-schemas 
                           (mapcar #'id (reverse db-schemas))
                           (list (id (lookup-schema sc (if (symbolp class) (find-class class) class)))))))
      (unless (class-indexing-enabled-p classobj)
        (cerror "Ignore and return nil"
                "Class ~A is not indexed" classname)
        (return-from map-class nil))
;;      (dump-schema-status sc classname)
      (loop for schema-id in schema-ids appending
           (map-index (if oids #'map-oid-fn #'map-fn)
                      (class-index sc)
                      :value schema-id
                      :collect collect)))))

(defun map-inverted-index (fn class index &rest args &key start end (value nil value-p) from-end collect oids)
  "map-inverted-index maps a function of two variables, taking key
   and instance, over a subset of class instances in the order
   defined by the index.  Specify the class by classname or class object 
   and index by quoted name.  The index may be a slot index, derived index,
   or a valued association slot.

   To map only a subset of key-value pairs, specify the range
   using the :start and :end keywords; all elements greater than
   or equal to :start and less than or equal to :end will be
   traversed regardless of whether the start or end value is in
   the index.  

   Use nil in the place of start or end to specify the first
   element or last element, respectively.  

   To map a single value, iff it exists, use the :value keyword.
   This is the only way to travers all nil values.

   To map from :end to :start in descending order, set :from-end
   to true.  If :value is used, :from-end is ignored

   The 'oids' argument passes the oid of the instance to the provided
   function instead of the recreated instance."
  (declare (dynamic-extent args)
           (ignorable args))
  (let* ((btree (if (symbolp index)
                    (find-inverted-index class index)
                    index))
         (class-obj (etypecase class
                      (symbol (find-class class))
                      (stored-class class)))
         (sc (get-store btree)))
    (flet ((map-obj (value oid)
             (funcall fn value (store-recreate-instance sc oid))))
      (cond ((eq 'association-effective-slot-definition (type-of (find-slot-def-by-name class-obj index)))
             (map-btree (if oids fn #'map-obj) btree :value (oid value) :collect collect))
            (value-p (map-btree (if oids fn #'map-obj) btree :value value :collect collect))
            (t (map-btree (if oids fn #'map-obj) btree :start start :end end :from-end from-end :collect collect))))))

(defun get-unique-values (index &aux values)
    (with-btree-cursor (cur index)
      (multiple-value-bind (valid? value oid)
          (cursor-first cur)
        (declare (ignore oid))
        (when valid?
          (push value values)
          (loop 
               (multiple-value-bind (valid? value oid)
                   (btree::cursor-next-nodup cur)
                 (declare (ignore oid))
                 (unless valid?
                   (return-from get-unique-values (nreverse values)))
                 (push value values)))))))

(defmethod sb-sequence:emptyp ((btree btree))
    (with-btree-cursor (cur btree)
      (multiple-value-bind (valid k) (cursor-next cur)
        (declare (ignore k))
        (cond ((not valid) ;; truly empty
               t)
              ((eq btree (store-root (get-store btree)))
               (not (cursor-next cur)))
              (t nil)))))

(defmethod find-inverted-index ((class symbol) slot &key (null-on-fail nil) (sc *store*))
  (find-inverted-index (find-class class) slot :null-on-fail null-on-fail :sc sc))

(defmethod find-inverted-index ((class stored-class) slot &key ignore-errors (store *store*))
  (ensure-finalized class)
  (flet ((assert-error ()
           (when ignore-errors (return-from find-inverted-index nil))
           (cerror "Return null and continue?"
                   "Inverted slot index ~A not found for class ~A with indexed slots: ~A" 
                   slot (class-name class) (indexed-slot-names class))))
    (let ((slot-def (find-slot-def-by-name class slot)))
      (unless (and slot-def
                   (or (eq (type-of slot-def) 'indexed-effective-slot-definition)
                       (eq (type-of slot-def) 'derived-index-effective-slot-definition)))
        (assert-error))
      (let ((idx (get-slot-def-index slot-def store)))
        (unless idx
          (setf idx (ensure-slot-def-index slot-def store)))
        idx))))


;;; DB Evolution
(defmethod upgrade-all-memory-instances ((sc store))
  "Touch each instance in memory to force update-instance-for-redefined class to
be called on classes that were just redefined. This in turn calls
upgrade-instance. This should be called after a redefinition."
  (loop for inst-pointer being the hash-value of (instance-cache sc)
        for inst = (sb-ext:weak-pointer-value inst-pointer)
        do (oid inst)))

(defmethod upgrade-all-db-instances ((sc store) class-schema)
  "Scan and upgrade each instance of the class referred to
by CLASS-SCHEMA. If there is a predecessor class in the database, its
instances are upgraded to the current. If the db-schema and class schema do
not match (i.e. we are connecting to a store) then go ahead and run
synchronize-store-class to upgrade class-level info like indices."
  (let* ((classname (schema-class-name class-schema))
     (db-schema (get-current-db-schema sc classname)))
    ;; When the db-schema is not up to date, make it so
    (unless (match-schemas class-schema db-schema)
      (synchronize-store-class sc (find-class classname) class-schema db-schema))
    ;; Update the instances oldest to newest
    (loop for schema in (get-db-schemas sc classname)
     unless (eq (id schema) (id db-schema)) do
     (progn
       (map-index (lambda (cidx pcidx oid)
            (declare (ignore cidx pcidx))
            (let ((instance (store-recreate-instance sc oid classname)))
              (upgrade-db-instance instance db-schema schema nil)))
                  (class-index sc)
              :value (id schema))
       (awhen (schema-successor (get-store-schema sc (id schema)))
         (awhen (get-store-schema sc it)
           (setf (schema-predecessor it) nil)))))))
;;	   (remove-controller-schema sc (schema-id schema))))))

(defmethod upgrade-db-instance ((instance stored-object) (new-schema upgradable-schema) (old-schema upgradable-schema) old-values)
  "Upgrade a database instance from the old-schema to the new-schema.
   This does mean loading it into memory (for now)!"
  (let ((st (get-store instance))
        (diff (schema-diff new-schema old-schema)))
    (awhen (upgrade old-schema)
      (apply-schema-change-fn instance it old-schema))
    (loop for entry in diff do
             (upgrade-instance-slot st instance (diff-type entry) (diff-recs entry) old-values))
    (initialize-new-slots instance diff)
    (set-instance-schema-id st (oid instance) (id new-schema))))

(defmethod upgrade-instance-slot (sc instance (type (eql :change)) recs old-values)
  "Handle changes in class type"
  (destructuring-bind (old-rec new-rec) recs
    (with-slots ((old-type type) (old-name name) (old-args args)) old-rec
      (cond ;; If it was not indexed, and now is, we have to notify the index of the new value
            ((and (member old-type '(:stored :cached))
                  (eq (slot-field-type new-rec) :indexed)
                  (slot-boundp instance old-name))
             (setf (slot-value instance old-name) (slot-value instance old-name)))
            ;; If it was indexed, and the base index has changed 
            ;; The new index will get updated as a natural part of the rest of the protocol
            ((and (member old-type '(:indexed :derived))
                  (not (eq (getf old-args :base)
                           (getf (slot-field-args new-rec) :base)))
                  (slot-boundp instance old-name))
             (let ((slot-value (slot-value instance old-name)))
               (unindex-slot-value sc slot-value (oid instance) old-name (getf old-args :base))))
            ;; If it was a stored slot and now isn't, drop it and add the new type back
            ((and (member old-type '(:stored :indexed :cached :derived))
                  (not (member (slot-field-type new-rec) '(:stored :indexed :cached :derived))))
             (upgrade-instance-slot sc instance :rem (list old-rec) old-values)
             (upgrade-instance-slot sc instance :add (list new-rec) old-values))
            ;; If the old slot was indexed
            ((and (eq old-type :indexed) (eq (slot-field-type new-rec) :indexed)
                  (not (eq (getf (slot-field-args old-rec) :base)
                           (getf (slot-field-args new-rec) :base))))
             nil)
            (t nil)))))

(defmethod upgrade-instance-slot (sc instance (type (eql :rem)) recs old-values)
  "Handle slot removal and cleanup of values, such as sets"
  (with-slots (type name args) (first recs)
    (when (member type '(:stored :cached :indexed :derived))
      (stored-slot-makunbound sc instance name))
    (when (member type '(:indexed :derived))
      (awhen (getf old-values name)
        (unindex-slot-value sc (cdr it) (oid instance) name args)))
    (when (eq type :set-valued)
      (let ((set (and (stored-slot-boundp sc instance name)
                      (stored-slot-reader sc instance name))))
        (when set (drop-btree set))
        (slot-makunbound instance name)))))

(defmethod upgrade-instance-slot (sc instance (type (eql :add)) recs old-values)
  "Not needed, new slots are initialized above"
  (declare (ignore sc instance recs old-values))
  nil)

(defun initialize-new-slots (instance diff)
  (labels ((adding-stored? (entry)
             (when (and (eq :add (diff-type entry))
                        (member (slot-field-type (first (diff-recs entry)))
                                '(:stored :indexed :cached :set-valued)))
               (slot-field-name (first (diff-recs entry)))))
           (change-to-stored? (entry)
             (when (and (eq :change (diff-type entry))
                        (not (member (slot-field-type (first (diff-recs entry)))
                                     '(:stored :indexed :cached :set-valued)))
                        (member (slot-field-type (second (diff-recs entry)))
                                '(:stored :indexed :cached :set-valued)))
               (slot-field-name (second (diff-recs entry)))))
           (init-slot? (entry)
             (or (adding-stored? entry)
                 (change-to-stored? entry)))
           (compute-init-slots ()
             (remove-if #'null (mapcar #'init-slot? diff))))
    (apply #'shared-initialize instance (compute-init-slots) nil)))

(defmethod change-db-instance ((current stored-object) previous
                               new-schema old-schema)
  "Change a database instance from one schema & class to another. These are
different objects with the same oid."
  (let ((sc (get-store current))
        (oid (oid current))
        (diff (schema-diff new-schema old-schema)))
      ;; do we need to pass the stored object?  Transient ops require previous?
      (awhen (upgrade old-schema)
        (apply-schema-change-fn current it old-schema))
      ;; Handle changed slots
      (loop for entry in diff do
           (change-instance-slot sc current previous (diff-type entry) (diff-recs entry)))
      ;; Initialize new slots (is this done by default?)
      (initialize-new-slots current diff)
      (uncache-instance sc oid)
      (set-instance-schema-id sc oid (id new-schema))))

(defmethod change-instance-slot (sc current previous (type (eql :change)) recs)
  "Handle changes in class type"
;; TODO
;;   (print recs)
;;   (dump-btree (instance-index sc))
;;   (dump-index (index-root sc))
  (destructuring-bind (old-rec new-rec) recs
    (with-slots ((old-type type) (old-name name) (old-args args)) old-rec
      (with-slots ((new-type type) (new-name name) (new-args args)) new-rec
        (cond ;; If it was not indexed, and now is, we have to notify the index of the new value (?)
          ((and (member old-type '(:stored :cached))
                (eq new-type :indexed) (slot-boundp previous old-name))
           (setf (slot-value previous old-name) (slot-value previous old-name)))
          ;; If the old slot was indexed, we definitely need to unindex it to avoid
          ;; having the objects hang around in the index
          ((and (eq old-type :indexed) (eq new-type :indexed)
                (slot-boundp previous old-name))
           (unindex-slot-value sc (slot-value previous old-name)
                               (oid previous) old-name (getf old-args :base))
           (setf (slot-value current new-name) (slot-value previous old-name)))
          ((and (eq old-type :indexed) (slot-boundp previous old-name))
           (unindex-slot-value sc (slot-value previous old-name)
                               (oid previous) old-name (getf old-args :base)))
          ;; If it was a stored slot and now isn't, drop it and add the new type back
          ((and (member old-type '(:stored :indexed :cached))
                (not (member new-type '(:stored :indexed :cached))))
           (change-instance-slot sc current previous :rem (list old-rec))
           (change-instance-slot sc current previous :add (list new-rec)))
          (t nil))))))

(defmethod change-instance-slot (sc current previous (type (eql :rem)) recs)
  "Handle slot removal and cleanup of values, such as sets"
  (declare (ignore current))
  (with-slots ((prev-type type) (prev-name name) (prev-args args)) (first recs)
    (cond ((member prev-type '(:stored :cached :indexed))
           (slot-makunbound previous prev-name))
          ((eq type :set-valued)
           (let ((set (and (stored-slot-boundp sc previous prev-name)
                           (stored-slot-reader sc previous prev-name))))
             (when set (drop-btree set))
             (slot-makunbound previous prev-name))))))

(defmethod change-instance-slot (sc current previous (type (eql :add)) recs)
  "Not needed, new slots are initialized above"
  (declare (ignore sc current previous recs))
  nil)

(defun warn-about-dropped-slots (op class names)
  (when (and *warn-when-dropping-stored-slots* names)
    (cerror "Drop the slots" 
        'dropping-stored-slot-data
        :operation op
        :class class
        :slots names)))

(define-condition dropping-stored-slot-data (warning)
  ((operation :initarg :operation)
   (class :initarg :class)
   (slots :initarg :slots))
  (:report (lambda (c stream)
         (with-slots (class slots operation) c
           (format stream "Dropping slot(s) ~A for class ~A in ~A. Continue the synchronization process?"
               slots class operation)))))

(defun warn-on-reinitialization-data-loss (class)
  "Warnings at class def time:
   - set-valued/assoc (warn!)
   - stored/indexed/cached (warn?)
   - derived hints?
   Be nice to be able to restore the slots rather than just
   avoid updating"
  (let* ((old-schema (get-class-schema class))
         (new-schema (class-instance-schema class))
         (diffs (schema-diff new-schema old-schema)))
    (dolist (diff diffs)
      (when (eq (diff-type diff) :rem)
        (warn-about-dropped-slots :rem class
                                  (mapcar #'slot-field-name (cdr diff)))))))

;;; Controller Protocol
(defgeneric open-store (st &key recover recover-fatal thread &allow-other-keys)
  (:documentation "Opens the underlying environment and all the necessary
database tables. Different data stores may use different keys so all methods
should &allow-other-keys. There are three standard keywords: :recover,
:recover-fatal and :thread. Recover means that recovery should be checked for
or performed on startup. Recover fatal means a full rebuild from log files is
requested. Thread merely indicates to the data store that it is a threaded
application and any steps that need to be taken (for example transaction
implementation) are taken. :thread is usually true."))

(defgeneric close-store (st)
  (:documentation "Close the db handles and environment. Should be in a state where lisp could be
shut down without causing an inconsistent state in the db. Also, the object
could be used by open-store to reopen the database."))

(defgeneric optimize-layout (st &key &allow-other-keys)
  (:documentation "If supported, speed up the index and allocation by freeing up any available
storage and return it to the free list. See the methods of data stores to
determine what options are valid. Supported both on stores (all btrees and
stored slots) and specific btrees."))


;;; Controller User API

;; start stop

;; (defun close-all-stores ()
;;  (loop for pair in *stores*
;;       do (close-store (cdr pair))))

;; (pushnew 'close-all-stores sb-ext:*exit-hooks*)

(defmacro with-open-store (spec &body body)
  "Executes the body with an open store,
   unconditionally closing the store on exit."
  `(let ((*store* nil))
     (declare (special *store*))
     (open-store ,spec)
     (unwind-protect
          (progn ,@body)
       (close-store *store*))))

(defmacro with-store ((store) &body body)
  (with-gensyms (ref)
    `(let* ((,ref ,store)
            (*store* 
             (if (listp ,ref)
                 (get-store ,ref)
                 ,ref)))
       (declare (special *store*))
       ,@body)))

;; drop-instances

;;; Root indexes
(defun add-to-root (key value &key (st *store*))
  "Add an arbitrary stored thing to the root, so you can retrieve it in a later
session. Anything referenced by an object added to the root is considered
reachable and thus live"
  (declare (type store st))
  ;; (assert (not (eq key *elephant-properties-label*)))
  (setf (get-value key (store-root st)) value))

(defun get-from-root (key &key (st *store*))
  "Get the value associated with key from the root.  Returns two
   values, the value, or nil, and a boolean indicating whether a
   value was found or not (so you know if nil is a value or an
   indication of non-presence)"
  (declare (type store st))
  (get-value key (store-root st)))

(defun root-existsp (key &key (st *store*))
  "Test whether a given key is instantiated in the root"
  (declare (type store st))
  (if (btree:existsp key (store-root st))
      t 
      nil))

(defun remove-from-root (key &key (st *store*))
  "Remove something from the root by the key value"
  (declare (type store st))
  (delete-key key (store-root st)))

(defun map-root (fn &key (st *store*))
  "Takes a function of two arguments, key and value, to map over
   all key-value pairs in the root"
  (map-btree fn (store-root st)))

;;; Slot Access
(defmethod slot-value-using-class ((class stored-class) (instance stored-object) (slot-def stored-slot-definition))
  "Get the slot value from the database."
  (let ((name (slot-definition-name slot-def)))
    (stored-slot-reader (get-store instance) instance name)))

(defmethod (setf slot-value-using-class) (new-value (class stored-class) (instance stored-object) (slot-def stored-slot-definition))
  "Set the slot value in the database."
  (let ((name (slot-definition-name slot-def)))
      (cond
        ((derived-slot-triggers slot-def)
         (stored-slot-writer (get-store instance) new-value instance name)
         (derived-index-updater class instance slot-def))
        (t (stored-slot-writer (get-store instance) new-value instance name))))
  new-value)

(defmethod slot-boundp-using-class ((class stored-class) (instance stored-object) (slot-def stored-slot-definition))
  "Checks if the slot exists in the database."
  (when instance
    (let ((name (slot-definition-name slot-def)))
      (stored-slot-boundp (get-store instance) instance name))))

(defmethod slot-boundp-using-class ((class stored-class) (instance stored-object) (slot-name symbol))
  "Checks if the slot exists in the database."
  (loop for slot in (class-slots class)
     for matches-p = (eq (slot-definition-name slot) slot-name)
     until matches-p
     finally (return (if (and matches-p
                              (subtypep (type-of slot) 'stored-slot-definition))
                       (stored-slot-boundp (get-store instance) instance slot-name)
                       (call-next-method)))))

(defmethod slot-makunbound-using-class ((class stored-class) (instance stored-object) (slot-def stored-slot-definition))
  "Removes the slot value from the database."
  (stored-slot-makunbound (get-store instance) instance (slot-definition-name slot-def)))

(defun valid-stored-reference-p (object sc)
  "Ensures that object can be written as a reference into store sc"
  (or (not (slot-boundp object 'spec))
      (eq (spec object) (spec sc))))

(define-condition cross-store-error (error)
  ((object :accessor error-object :initarg :object)
   (home :accessor error-home-store :initarg :home-store)
   (guest :accessor error-guest-store :initarg :guest-store))
  (:documentation "An error condition raised when an object is being written into a data store other
                   than its home store")
  (:report (lambda (condition stream)
             (format stream "Attempted to write object ~A with home store ~A into store ~A"
                     (error-object condition)
                     (error-home-store condition)
                     (error-guest-store condition)))))

(defun signal-cross-store-error (object sc)
  (cerror "Proceed to write incorrect reference"
          'cross-reference-error
          :object object
          :home-store (get-store object)
          :guest-store sc))

;;; Macros
(defmacro defstore (name super spec &rest options)
  "Define a new STORE class.")

;; TODO 2024-12-05: do we want to pass DB by value here (in the environment of
;; WITH-STORE) or are we better off binding DATABASE instances as
;; *STORE-BACKEND*?
(defmacro with-store ((sym &rest initargs &key &allow-other-keys) &body body)
  "Similar to WITH-DB but for STORE objects instead of DATABASEs. 

INITARGS may contain any number keys that have been registered with the
current *STORE-BACKEND*.")
