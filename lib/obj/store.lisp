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

(defparameter *return-null-on-missing-instance* t
  "During instance recreation, references to missing instances
simply return null instead of signaling an error.")

(defvar *store-spec* nil)
(defvar *store-lock* (make-mutex :name "STORE"))

(defvar *store-code-version* '(1 0 0)
  "The current database version supported by the code base.")
(defvar *store-code-version-int* 100
  "The current database version supported by the code base.")
(defvar *store-unmarked-code-version* '(1 0 0)
  "If a database is opened with existing data but no version then
   we assume it's version 1.0.0")

;;; Stored Set
;; default implementation of simple sets using btrees
(defclass sset (stored-collection) ()
  (:documentation "An unordered stored set of unique elements according to serializer
equal comparison"))

(defgeneric insert-item (item sset)
  (:documentation "Insert a new item into the sset"))

(defgeneric remove-item (item sset)
  (:documentation "Remove specified item from sset"))

(defgeneric map-sset (fn sset)
  (:documentation "Map operator for ssets"))

(defgeneric find-item (item sset &key key test)
  (:documentation "Find a an item in the sset using key and test"))

(defgeneric sset-list (sset)
  (:documentation "Convert items of sset into a list for processing"))

(defgeneric build-sset (sc)
  (:documentation "Construct an empty default sset or backend specific sset.
                   This is an internal function used by make-sset"))

(defgeneric drop-sset (sset)
  (:documentation "Release sset storage to database for reuse"))

(defsclass default-sset (sset)
  ((btree :accessor sset-btree :initarg :btree)))

(defmethod drop-instance ((sset sset))
  (drop-sset sset)
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
  "Make sure we reclaim the sset storage"
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

(defsclass default-slot-set (slot-set default-sset) ()
  (:documentation "A default slot-set implementation"))

(defgeneric build-slot-set (sc)
  (:documentation "Construct an empty default sset or backend specific sset.
                   This is an internal function used by make-sset"))

(defgeneric slot-set-list (slot-set)
  (:documentation "Convert items of sset into a list for processing")
  (:method ((set default-slot-set))
    (sset-list set)))

(defgeneric map-slot-set (fn slot-set)
  (:documentation "Map operator for ssets")
  (:method (fn (set default-slot-set))
    (map-sset fn set)))

(defgeneric drop-slot-set (sset)
  (:documentation "Release sset storage to database for reuse")
  (:method ((set default-slot-set))
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

;; Handling reads
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

;;  Handling updates
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
   (version :type fixnum :accessor version :initform 100)
   ;; Generic support for the object, indexing and root protocols
   (root 
    :reader store-root 
    :documentation "This is an instance of the data store btree. It should have an OID that is
fixed in the code and does not change between sessions. Usually this is
something like 0, 1 or -1")
   (schema-table 
    :reader schema-table
    :initarg :schema-table
    :documentation "Schema id to schema database table")
   (schema-name-index
    :reader schema-name-index
    :initarg :schema-name-index
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
    :initarg :instance-index
    :documentation "OID->CID table.")
   (class-index 
    :initarg :class-index
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
   (ser :accessor ser :initform nil)
   (de :accessor de :initform nil))
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
  (let ((cid (oid-to-schema-id oid st)))
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
  "Method called by the deserializer to return an instance."
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
      (unless *return-null-on-missing-instance*
        (signal e)))))

(defmethod get-slot-def-index ((def association-effective-slot-definition) sc)
  "Since endpoints of an association implement an index we should be able to perform
   inverted-index relation functions on them directly"
  (get-association-index def sc))

(defmethod cache-instance ((sc store) obj)
  "Cache a stored object with the controller."
  (declare (type store sc))
  (setf (get-cache (oid obj) (instance-cache sc)) obj))

(defmethod get-cached-instance ((sc store) oid)
  "Get a cached instance, or instantiate!"
  (declare (type store sc)
           (type fixnum oid))
  (awhen (get-cache oid (instance-cache sc))
    it))

(defmethod uncache-instance ((sc store) oid)
  (with-mutex ((instance-cache-lock sc))
    (remhash oid (instance-cache sc))))

(defmethod flush-instance-cache ((sc store))
  "Reset the instance cache (flush object lookups).  Useful
for testing.  Does not reclaim existing objects so there will be duplicate
instances with identical functionality"
  (with-mutex ((instance-cache-lock sc))
    (setf (instance-cache sc) (make-cache-table :test 'eql))))

(defun register-new-instance (instance class store)
  (setf (oid instance) (next-oid store))
  (register-instance store class instance))

(defun check-valid-store (store)
  (if-let ((ok (subtypep (type-of store) 'store)))
    ok
    (error "This function requires a valid store")))

(defmethod build-sset ((sc store))
  "Default sset method; override if backend has better policy"
  (let ((btree (make-dup-btree sc)))
    (make-instance 'default-sset :btree btree :store sc)))

(defun make-sset (&key items sset (store *store*))
  (let ((new-sset (build-sset store)))
    (when (and items sset)
      (error "Can only initialize a new sset with item list or sset to copy, not both"))
    (when items
      (mapc (lambda (item)
              (insert-item item new-sset))
            items))
    (when sset
      (map-sset (lambda (item)
                  (insert-item item new-sset))
                sset))
    new-sset))

(defmethod insert-item (item (sset default-sset))
  (setf (get-value item (sset-btree sset)) t)
  item)

(defmethod remove-item (item (sset default-sset))
  (delete-key (sset-btree sset) item)
  item)

(defmethod find-item (item (sset default-sset) &key key (test #'equal))
  (if (not (or key test))
      (get-value item (sset-btree sset))
      (map-btree (lambda (elt dc)
                   (declare (ignore dc))
                   (let ((cmpval (if key (funcall key elt) elt)))
                     (if (funcall test item cmpval)
                         (return-from find-item elt))))
                 (sset-btree sset))))

(defmethod map-sset (fn (sset default-sset))
  (map-btree (lambda (key value) 
               (declare (ignore value))
               (funcall fn key))
             (sset-btree sset))
  sset)

(defmethod sset-list ((sset default-sset))
  (map-btree #'(lambda (k v) 
                 (declare (ignore v))
                 k) 
             (sset-btree sset) :collect t))

(defmethod drop-sset ((sset default-sset))
  (ensure-transaction (:store *store*)
    (awhen (sset-btree sset)
      (drop-btree it))))

(defmethod build-slot-set ((sc store))
  (let ((btree (make-btree sc)))
    (make-instance 'default-slot-set :btree btree :store sc)))

(defmethod drop-instance ((inst stored-object))
  (drop-instance-slots inst)
  (call-next-method))

(defun drop-instances (instances &key (store *store*) (txn-size 500))
  "Removes a list of stored objects from all class indices and unbinds any
stored slot values associated with those instances."
  (declare (optimize (speed 1) (debug 3) (safety 3)))
  (awhen (ensure-list instances)
    (assert (consp it))
    (do-subsets (subset txn-size it)
      (ensure-transaction (:store store)
        (mapc #'drop-instance subset)))))

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

(defgeneric oid-to-schema-id (oid st)
  (:method (oid (st store))
    (get-value oid (instance-index st))))

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

(defun slot-index-sane-p (sc class slotname &key errorp)
  (declare (optimize (safety 3))
           (store sc)
           ((or class symbol) class)
           (symbol slotname))
  (ensure-finalized (etypecase class (class class) (symbol (find-class class))))
  (flet ((exit (fmt &rest args)
           (if errorp
               (apply #'error fmt args)
               (return-from slot-index-sane-p
                 (values nil (apply #'format nil fmt args))))))
    (let* ((*store* sc)
           (objects<-class-index (remove-if-not (lambda (obj)
                                                  (slot-boundp obj slotname))
                                                (get-instances-by-class class)))
           (objects<-inverted-index (map-inverted-index
                                     (lambda (key inst)
                                       (cond
                                         ((not (slot-exists-p inst slotname))
                                          (warn "Slot ~S is missing from obj ~S, ignoring." slotname inst)
                                          inst)
                                         ((not (slot-boundp inst slotname))
                                          (exit "Slot ~S is unbound in obj ~S but present in the index with key ~S"
                                                slotname inst key))
                                         ((not (compare-equal key (slot-value inst slotname)))
                                          (exit "The value ~S of slot ~S in obj ~S disagrees with the index key ~S"
                                                (slot-value inst slotname) slotname inst key))
                                         (t inst)))
                                     class slotname :collect t))
           (diff (set-difference objects<-class-index objects<-inverted-index)))
      (unless (null diff)
        (exit "Objects are missing from the inverted index ~S for class ~S: ~S" slotname class diff))
      t)))

(defun slot-indices-sane-p (sc class &rest args)
  "Check slot index sanity for CLASS or all classes known to SC if
  CLASS is NIL."
  (let ((classes (ensure-list (etypecase class
                                (null (known-classes sc))
                                (class class)
                                (symbol (find-class class))))))
    (loop for class in classes
                                        ;do (format t "=== class ~S~%" class)
          collect (cons (class-name class)
                        (loop for slotname in (indexed-slot-names class)
                              for sane-p = (multiple-value-list
                                            (apply #'slot-index-sane-p sc class slotname args))
                                        ;do (format t "slot ~S~%" slotname)
                              collect (cons slotname sane-p))))))

(defun map-class (fn class &key collect oids (sc *store*))
  "Perform a map operation over all instances of CLASS. Takes a
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

(defmethod find-inverted-index ((class symbol) slot &key (null-on-fail nil) (store *store*))
  (find-inverted-index (find-class class) slot :null-on-fail null-on-fail :store store))

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

;; Main protocol

(defmethod initialize-instance :around ((instance stored-object) &rest initargs 
                                                                 &key (sc *store*) &allow-other-keys)
  "Ensure instance creation is inside a transaction, huge (5x) performance impact per object"
  (declare (ignore initargs))
  (assert sc nil "You must have an open store controller to create ~A" instance)
  (ensure-transaction (:store sc)
    (call-next-method)))

(eval-always
  (defun compute-bindings (class slots bindings)
    "Helper function for bind-slot-defs"
    (loop for (name accessor) in bindings collect
             `(,name (get-init-slotnames ,class #',accessor ,slots)))))

(defmacro bind-slot-defs (class slots bindings &body body)
  "Bindings contain name, accessor pairs.  Extract 
   slot-definitions into variable name using accessor and
   filter by the list of valid slots"
  (with-gensyms (classref slotrefs)
    `(let* ((,classref ,class)
            (,slotrefs ,slots)
            ,@(compute-bindings classref slotrefs bindings))
       ,@body)))

(defmethod shared-initialize :around ((instance stored-object) slot-names &rest initargs &key from-oid &allow-other-keys)
  "Initializes the stored slots via initargs or forms.
This seems to be necessary because it is typical for implementations to
optimize setting the slots via initforms and initargs in such a way that
slot-value-using-class et al aren't used. We also handle writing any indices
after the class is fully initialized. Calls the next method for the transient
slots."
  (let ((class (class-of instance)))
    (bind-slot-defs 
        class slot-names
        ((transient-slots transient-slot-names)
         (cached-slots cached-slot-names)
         (indexed-slots indexed-slot-names)
         (derived-slots derived-index-slot-names)
         (association-end-slots association-end-slot-names)
         (stored-slots stored-slot-names))
      ;; Slot initialization
      (let* ((stored-initializable-slots 
               (union (union stored-slots indexed-slots) association-end-slots))
             (set-slots (get-init-slotnames class #'set-valued-slot-names slot-names)))
        ;;      NOTE: backing store for cached slots is only initialized on checkout or txn
        (cond (from-oid ;; If re-starting, make sure we read the cached values
                        nil)
              (t ;; If new instance, initialize all slots
                 (setq transient-slots (union transient-slots cached-slots))
                 (initialize-stored-slots class instance stored-initializable-slots initargs from-oid)))
        ;; Always initialize transients
        (apply #'call-next-method instance transient-slots initargs)
        ;; Initialize set slots after transient initialization
        (unless from-oid
          (initialize-set-slots class instance set-slots))
        (loop for dslotname in derived-slots do
                 (derived-index-updater class instance (find-slot-def-by-name class dslotname)))))))

(defun initialize-stored-slots (class instance stored-slot-inits initargs object-exists)
  (dolist (slotname stored-slot-inits)
    (let ((slot-def (find-slot-def-by-name class slotname)))
      (unless (or (initialize-from-initarg class instance slot-def 
                                           (slot-definition-initargs slot-def) initargs)
                  object-exists
                  (slot-boundp-using-class class instance slot-def))
        (awhen (slot-definition-initfunction slot-def)
          (setf (slot-value-using-class class instance slot-def)
                (funcall it)))))))

(defun initialize-set-slots (class instance set-slots)
  (declare (ignore class instance))
  (dolist (slotname set-slots)
    (declare (ignorable slotname))
    ;;    (setf (slot-value-using-class class instance
    ;;				  (find-slot-def-by-name class slotname))
    ;;	  nil)
    ))

(defun initialize-from-initarg (class instance slot-def slot-initargs initargs)
  (loop for slot-initarg in slot-initargs
        when (member slot-initarg initargs :test #'eq)
        do
           (setf (slot-value-using-class class instance slot-def)
                 (getf initargs slot-initarg))
           (return t)
        finally (return nil)))

(defun get-init-slotnames (class accessor slot-names)
  (let ((slotnames (funcall accessor class)))
    (if (not (eq slot-names t))
        (intersection slotnames slot-names :test #'equal)
        slotnames)))


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

(defvar *always-convert* nil)
(defparameter *legacy-symbol-conversions* nil)
(defparameter *no-deserialization-package-found-action* :warn
  "When eql :WARN Issue a warning when deserializing a symbol from a package that
is not created. Other possible values include :ERROR and :CREATE.")

(defun add-symbol-conversion (old-name old-package new-name new-package old-version)
  "Users can specify specific symbol conversions on upgrade prior to 
   migrating old databases"
  (declare (ignore old-version))
  (push (cons (cons old-name old-package) (cons new-name new-package)) *legacy-symbol-conversions*))

(defun map-legacy-symbols (symbol-string package-string old-version)
  (declare (ignore old-version))
  (let ((entry (assoc (cons (string-upcase symbol-string) (string-upcase package-string))
              *legacy-symbol-conversions* :test #'equal)))
    (if entry
    (values t (cadr entry) (cddr entry))
    nil)))

(defparameter *legacy-package-conversions* nil)

(defun add-package-conversion (old-package-string new-package-string old-version)
  "Users can specify wholesale package name conversions on upgrade 
   prior to migrating old databases"
  (declare (ignore old-version))
  (push (cons old-package-string new-package-string) *legacy-package-conversions*))

(defun map-legacy-package-names (package-string old-version)
  (declare (ignore old-version))
  (let ((entry (assoc (string-upcase package-string) *legacy-package-conversions* :test #'equal)))
    (if entry
    (cdr entry)
    package-string)))

(defun map-legacy-names (symbol-name package-name old-version)
  (multiple-value-bind (mapped? new-name new-package)
      (map-legacy-symbols symbol-name package-name old-version)
    (if mapped?
    (values new-name new-package)
    (values symbol-name (map-legacy-package-names package-name old-version)))))

(defun translate-and-intern-symbol (sc symbol-name package-name)
  "Service for the serializer to translate any renamed packages or symbols
   and then intern the decoded symbol."
  (if package-name
      (multiple-value-bind (sname pname)
      (if (or *always-convert* (not (= (version sc)
                                       *store-code-version-int*)))
          (map-legacy-names symbol-name package-name (database-version sc))
          (values symbol-name package-name))
    (let ((package (find-package pname)))
      (if package
          (intern sname package)
          (progn
        (case *no-deserialization-package-found-action*
          (:warn 
           (warn "Couldn't deserialize package ~A based on symbol ~A's home package ~A.
                         Creating an uninterned symbol" pname sname package-name))
          (:error
           (error "Couldn't deserialize package ~A based on symbol ~A's home package ~A."
              pname sname package-name))
          (:create
           (intern sname 
               (make-package pname :use '(cl))))
          (t nil))
        (make-symbol sname)))))
      (make-symbol symbol-name)))

;;; Controller Protocol
(defgeneric open-store (st &key recover recover-fatal &allow-other-keys)
  (:documentation "Open the store and all necessary database tables.
Different data stores may use different keys so all methods should
&allow-other-keys. The only standard keyword is RECOVER which means that
recovery should be checked for or performed on startup. When the value is
`:fatal' full rebuild from log files is requested."))

(defgeneric close-store (st)
  (:documentation "Close the store and underlying database tables.
Should be in a state where lisp could be shut down without causing an
inconsistent state in the db. Also, the object could be used by open-store to
reopen the database."))

(defgeneric optimize-layout (st &key &allow-other-keys)
  (:documentation "If supported, speed up the index and allocation by freeing up any available
storage and return it to the free list. See the methods of data stores to
determine what options are valid. Supported both on stores (all btrees and
stored slots) and specific btrees."))

;;; Controller User API

;; start stop

;; (defun close-all-stores () (maphash-values #'close-store *store-table*))

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

;;; Indexed slot access
(defmethod (setf slot-value-using-class)
    (new-value (class stored-class) (instance stored-object) (slot-def indexed-slot-definition))
  "Update indices when writing an indexed slot.  Make around method to ensure a single transaction
   for write + index update"
  (let ((store (get-store instance)))
    (ensure-transaction (:store store)
      (update-slot-index store class instance slot-def new-value)
      (call-next-method))))

(defmethod slot-makunbound-using-class ((class stored-class) (instance stored-object) (slot-def indexed-slot-definition))
  "Removes the slot value from the database."
  (let ((sc (get-store instance))
        (oid (oid instance)))
    (ensure-transaction (:store sc)
      (let* ((idx (get-slot-def-index slot-def sc))
             (old-value-bound-p (slot-boundp-using-class class instance slot-def))
             (old-value (when old-value-bound-p
                          (slot-value-using-class class instance slot-def))))
        (unless idx
          (setf idx (ensure-slot-def-index slot-def sc)))
        (when old-value-bound-p
          (remove-kv old-value oid idx)))
      (call-next-method))))

;;;; Derived slot index access
(defmethod (setf slot-value-using-class)
    (new-value (class stored-class) (instance stored-object) (slot-def derived-index-slot-definition))
  "Derived slot values are always set in response to a slot write"
  (declare (ignore new-value))
  (error "Cannot write computed (derived) slot ~A in ~A for class ~A; for read/index retrieval only"
         (slot-definition-name slot-def) instance (class-name class)))

(defmethod slot-makunbound-using-class ((class stored-class) (instance stored-object) (slot-def derived-index-slot-definition))
  "Unbinding cannot be performed explicitly.  It is effectively 
   inhibited when the derived fn says 'no'"
  (warn "Cannot unbind derived slot values for ~A in class ~A" 
        (slot-definition-name slot-def) (class-name class)))

;;; Cached slot access
(defsclass stored-cache-object (stored-object)
  ((pchecked-out :accessor pchecked-out-p :initform nil)
   (checked-out :accessor checked-out-p :initform nil :transient t))
  (:documentation "Adds a special value slot to store checkout state"))

(defmethod shared-initialize :around ((instance stored-cache-object) slot-names &key from-oid (make-cached-instance nil make-cached-instance-p) &allow-other-keys)
  ;; User asked us to start in cached mode?  Otherwise default to not.
  (when make-cached-instance-p
    (setf (slot-value instance 'pchecked-out) make-cached-instance
          (slot-value instance 'checked-out) make-cached-instance))
  (when (and from-oid (eq (get-cache-style (class-of instance)) :checkout))
    (unless make-cached-instance-p
      (setf (slot-value instance 'checked-out) 
            (slot-value instance 'pchecked-out)))
    (when (checked-out-p instance)
      (bind-slot-defs (class-of instance) slot-names
          ((cached-slots cached-slot-names))
        (refresh-cached-slots instance cached-slots))))
  (call-next-method))

(defmethod slot-value-using-class
    ((class stored-class) (instance stored-object) (slot-def cached-slot-definition))
  (case (%cache-style class)
    (:checkout
     (if (checked-out-p instance)
         (call-next-method)
         (stored-slot-reader (get-store instance) instance (slot-definition-name slot-def))))
    (:txn
     (stored-slot-reader (get-store instance) instance (slot-definition-name slot-def)))
    (t 
     (stored-slot-reader (get-store instance) instance (slot-definition-name slot-def)))))

(defmethod (setf slot-value-using-class)
    (new-value (class stored-class) (instance stored-object) (slot-def cached-slot-definition))
  "Always write local slot value; maybe write stored value if no caching or write-through"
  (case (%cache-style class)
    (:checkout
     (if (ignore-errors (checked-out-p instance))
         (call-next-method)
         (stored-slot-writer (get-store instance) new-value instance 
                             (slot-definition-name slot-def))))
    ;;	 (error "Cannot write to checkout-style cached objects when not checked out")))
    (t
     (stored-slot-writer (get-store instance) new-value instance 
                         (slot-definition-name slot-def)))))

(defmethod slot-boundp-using-class 
    ((class stored-class) (instance stored-object) (slot-def cached-slot-definition))
  "Checks if the slot exists in the database."
  (case (%cache-style class)
    (:checkout
     (if (checked-out-p instance)
         (call-next-method)
         (stored-slot-boundp (get-store instance) instance (slot-definition-name slot-def))))
    (t (stored-slot-boundp (get-store instance) instance (slot-definition-name slot-def)))))

(defmethod slot-makunbound-using-class 
    ((class stored-class) (instance stored-object) (slot-def cached-slot-definition))
  "Removes the slot value from the database."
  (case (%cache-style class)
    (:checkout
     (if (checked-out-p instance)
         (call-next-method)
         (stored-slot-makunbound (get-store instance) instance (slot-definition-name slot-def))))
    (t (stored-slot-makunbound (get-store instance) instance (slot-definition-name slot-def)))))

;;;; Cache mode and class-level ops
(defmethod caching-style ((class stored-class))
  (%cache-style class))

(defmethod (setf caching-style) (style (class stored-class))
  (case style
    ((or :checkout :txn)
     (unless (cached-slot-defs class)
       (error "Cannot enable caching for classes with no cached slots"))
     (setf (obj/meta/stored::%cache-style class) style))
    (:none 
     (setf (obj/meta/stored::%cache-style class) style))
    (t (error "Unknown caching mode ~A" style))))

(defmethod cached-class ((class stored-class))
  (when (cached-slot-defs class) t))

;;;; Cached instance ops
(defmethod stored-checked-out-p ((object stored-cache-object))
  (pchecked-out-p object))

(defmethod stored-checkout ((object stored-cache-object))
  "Set the checkout state and refresh the memory slots"
  (ensure-transaction ()
    (unless (eq (%cache-style (class-of object)) :checkout)
      (error "Class ~A for object ~A is not enabled for checkout.  (mode=~A)"
             (class-of object) object (%cache-style (class-of object))))
    (when (pchecked-out-p object)
      ;; This should be a condition that can fail silently?
      (error "Object ~A is already checked out" object))
    (setf (pchecked-out-p object) t) ;; grab write lock, rollback parallel txns
    ;; THIS IS BAD / READER ON OBJECT BEFORE CHECKOUT GETS STALE DATA
    ;; CAN WE BYPASS PROTOCOL TO WRITE MEMORY STORAGE DIRECTLY IN REFRESH?
    (setf (checked-out-p object) t)
    (refresh-cached-slots object (cached-slot-names (class-of object)))
    object))

(defmethod stored-sync ((object stored-cache-object))
  "Synchronize the slots to the database without a checkin"
  (ensure-transaction ()
    (assert (pchecked-out-p object))
    (flush-cached-slots object (cached-slot-names (class-of object)))
    object))

(defmethod maybe-stored-sync ((instance stored-object))
  nil)

(defmethod maybe-stored-sync ((instance stored-cache-object))
  "Synchronize the slots to the database without a checkin"
  (ensure-transaction ()
    (when (and (eq (get-cache-style (class-of instance)) :checkout)
               (checked-out-p instance))
      (stored-sync instance))))

(defmethod stored-checkout-cancel ((object stored-cache-object))
  (ensure-transaction ()
    (assert (pchecked-out-p object))
    (setf (pchecked-out-p object) nil)
    (setf (checked-out-p object) nil)))

(defmethod stored-checkin ((object stored-cache-object))
  "Flush the slot states to the database and release the checkout state.
   NOTE: Can this operation fail under concurrency if user enforces 
   single writer - e.g. checkin parallel with access, checkin parallel
   with attempted checkout?"
  (let ((checked-out t))
    (ensure-transaction ()
      (unless (eq (obj/meta/stored::%cache-style (class-of object)) :checkout)
        ;; TEST 2026-07-29: 
        (stored-checkout-cancel object)
        (error "Cannot checkin if class caching style is ~A. Canceling checkout." 
               (obj/meta/stored::%cache-style (class-of object))))
      (when (pchecked-out-p object)
        (setf (pchecked-out-p object) t) ;; establish a write lock
        (flush-cached-slots object (cached-slot-names (class-of object)))
        (setf (pchecked-out-p object) nil)
        (setf checked-out nil)))
    (setf (checked-out-p object) checked-out)
    object))

(defmacro with-stored-checkouts (objects &rest body)
  "Make sure objects are checked out in the body and are
   checked back in when the form returns.  This acts as
   a guard by "
  (with-gensyms (object objs)
    `(let ((,objs (list ,@objects)))
       (unwind-protect 
            (progn
              (dolist (,object ,objs)
                (stored-checkout ,object))
              ,@body)
         (dolist (,object ,objs)
           (stored-checkin ,object))))))

;;;; Cached slot value manipulation utils
(defun refresh-cached-slots (instance slots)
  "Assumes checkout mode is t so side effects are only
   in memory"
  (assert (pchecked-out-p instance))
  (assert (eq (%cache-style (class-of instance)) :checkout))
  (let ((sc (get-store instance)))
    (dolist (slot slots)
      (if (stored-slot-boundp sc instance slot)
          (setf (slot-value instance slot)
                (stored-slot-reader sc instance slot))
          (slot-makunbound instance slot)))))

(defun flush-cached-slots (instance slots)
  "Assumes object is checked out"
  (assert (pchecked-out-p instance))
  (let ((sc (get-store instance)))
    (dolist (slot slots)
      (if (slot-boundp instance slot)
          (stored-slot-writer sc (slot-value instance slot) instance slot)
          (stored-slot-makunbound sc instance slot)))))

;;; Set API
(defgeneric get-instances-by-class (stored-class)
  (:documentation "Retrieve all instances from the class index as a list of objects."))

(defgeneric get-instance-by-value (stored-class slot-name value)
  (:documentation "Retrieve instances from a slot index by value. 
Return only the first instance if there are duplicates."))

(defgeneric get-instances-by-value (stored-class slot-name value)
  (:documentation "Return a list of all instances where the slot value is equal to value."))

(defgeneric get-instances-by-range (stored-class slot-name start end)
  (:documentation "Returns a list of all instances that match values between start and end.
An argument of nil to start or end indicates, respectively, the lowest or
highest value in the index"))

(defun identity2 (k v)
  (declare (ignore k))
  v)

(defun identity3 (k v pk)
  (declare (ignore k pk))
  v)

(defmethod get-instances-by-class ((class symbol))
  (get-instances-by-class (find-class class)))

(defmethod get-instances-by-class ((class stored-class))
  (map-class #'identity class :collect t))

(defmethod get-instances-by-value ((class symbol) slot-name value)
  (get-instances-by-value (find-class class) slot-name value))

(defmethod get-instances-by-value ((class stored-class) slot-name value)
  (declare (type (or string symbol) slot-name))
  (map-inverted-index #'identity2 class slot-name :value value :collect t))

(defmethod get-instance-by-value ((class stored-class) slot-name value)
  (awhen (find-inverted-index class slot-name)
    (multiple-value-bind (oid found?)
        (get-value value it)
      (when found?
        (store-recreate-instance (get-store it) oid)))))

(defmethod get-instance-by-value ((class symbol) slot-name value)
  (get-instance-by-value (find-class class) slot-name value))

(defmethod get-instances-by-range ((class symbol) slot-name start end)
  (get-instances-by-range (find-class class) slot-name start end))

(defmethod get-instances-by-range ((class stored-class) idx-name start end)
  (declare (type (or number symbol string null) start end)
           (type symbol idx-name))
  (map-inverted-index #'identity2 class idx-name :start start :end end :collect t))

;;; Serde
(declaim  (optimize (speed 3) (safety 0) (space 0) (debug 0)))
(defconstant +fixnum32+              1)
(defconstant +fixnum64+              2)
(defconstant +char+                  3)
(defconstant +single-float+          4)
(defconstant +double-float+          5)
(defconstant +negative-bignum+       6)
(defconstant +positive-bignum+       7)
(defconstant +rational+              8)
;; split strings and encoding
(defconstant +utf8-string+           9)
(defconstant +utf16-string+         10)
(defconstant +utf32-string+         11)
;; String-based aggregates
(defconstant +pathname+             12)
(defconstant +symbol+               13)
;; Stored by ID (requires instance table)
(defconstant +stored-ref+       14)
;; Stored by id+classname
(defconstant +stored+           15)
;; Composite objects
(defconstant +cons+                 16)
(defconstant +hash-table+           17)
(defconstant +object+               18)
(defconstant +array+                19)
(defconstant +struct+               20)
(defconstant +class+                21)
(defconstant +complex+              22)
;;(defconstant +oid-pair+             23)

(defconstant +short-float+          30)

(defconstant +nil+                  #x3F) ;63
(defconstant +reserved-dbinfo+      #xF0) ;240

;; Arrays
(defconstant +fill-pointer-p+     #x20)
(defconstant +adjustable-p+       #x40)

(defconstant +2^31+ (expt 2 31))
(defconstant +2^32+ (expt 2 32))
(defconstant +2^63+ (expt 2 63))
(defconstant +2^64+ (expt 2 64))

;; Circularity Hash for Serializer
(defvar *circularity-initial-hash-size* 50
  "This is the default size of the circularity cache used in the serializer.")

(defparameter *circularity-hash-queue* (make-array 20 :fill-pointer 0 :adjustable t)
  "Circularity ids for the serializer.")

(defparameter *serializer-lock* (sb-thread:make-mutex :name "serializer"))

(defun get-circularity-hash ()
  "Get a clean hash for object serialization"
  (declare (type fixnum *circularity-initial-hash-size*))
  (or
   (sb-thread:with-mutex (*serializer-lock*)
     (and (plusp (length *circularity-hash-queue*))
          (vector-pop *circularity-hash-queue*)))
   (make-hash-table :test 'eq :size *circularity-initial-hash-size*)))

(defun release-circularity-hash (hash)
  "Return the hash to the queue for reuse"
  (unless (= (hash-table-count hash) 0)
    (clrhash hash))
  (sb-thread:with-mutex (*serializer-lock*)
    (vector-push-extend hash *circularity-hash-queue*)))

;; Circularity Hash for Deserializer

;; NOTE: this strategy may create GC problems as it maintains references to
;; potentially large objects
(defparameter *circularity-vector-queue* (make-array 20 :fill-pointer 0 :adjustable t)
  "A list of vectors used for linear deserialization.
This works nicely because all ID's are written in integer order to the stream,
so we can just write the next one into the array already knowing what the ID
is.")

(defun get-circularity-vector ()
  "Get a fresh vector"
  (or (sb-thread:with-mutex (*serializer-lock*)
        (and (plusp (length *circularity-vector-queue*))
             (vector-pop *circularity-vector-queue*)))
      (make-array 50 :element-type t :initial-element nil 
                     :fill-pointer 0 :adjustable t)))

(defun release-circularity-vector (vector)
  "Don't need to erase, just reset fill-pointer as it 
   determines extent of valid data"
  (setf (fill-pointer vector) 0)
  (sb-thread:with-mutex (*serializer-lock*)
    (vector-push-extend vector *circularity-vector-queue* 20)))

;; Unicode
(defun serialize-string (string bstream)
  "Try to write each format type and bail if code is too big"
  (declare (type buffer-stream bstream)
           (type string string))
  (cond ((and (not (equal "" string)) (> (char-code (char string 0)) #xFFFF))
         (serialize-to-utf32le string bstream))
        ;; Accelerate the common case where a character set is not Latin-1
        ((and (not (equal "" string)) (> (char-code (char string 0)) #xFF))
         (or (serialize-to-utf16le string bstream)
             (serialize-to-utf32le string bstream)))
        ;; Actually code pages > 0 are rare; so we can pay an extra cost
        (t (or (serialize-to-utf8 string bstream)
               (serialize-to-utf16le string bstream)
               (serialize-to-utf32le string bstream)))))

(defun serialize-to-utf8 (string bstream)
  "Standard serialization"
  (declare (type buffer-stream bstream)
           (type string string))
  (with-slots (buffer size (allocated length)) bstream
    (declare ;; (type array-or-pointer-char buffer)
             (type fixnum size allocated))
    (let* ((saved-size (the fixnum (size bstream)))
           (saved-pos (the fixnum (offset bstream)))
           (characters (the fixnum (length string))))
      (labels ((fail () 
                 (setf (size bstream) saved-size)
                 (setf (offset bstream) saved-pos)
                 (return-from serialize-to-utf8 nil))
               (succeed ()
                 (return-from serialize-to-utf8 t)))
        (buffer-write-byte +utf8-string+ bstream)
        (buffer-write-int32 characters bstream)
        (let ((needed (the fixnum (+ size characters))))
          (declare (type fixnum needed))
          (when (the boolean (> needed allocated))
            (resize-buffer-stream bstream needed))
          (etypecase string
            (simple-string
             (loop for i fixnum from 0 below characters do
                      (let ((code (the fixnum 
                                       (char-code 
                                        (the character (schar string i))))))
                        (declare (type fixnum code))
                        (when (the boolean (> code #xFF)) (fail))
                        (setf (deref buffer
                                     (the fixnum (+ i size))) 
                              code))))
            (string
             (loop for i fixnum from 0 below characters do 
                      (let ((code (the fixnum
                                       (char-code 
                                        (the character (char string i))))))
                        (declare (type fixnum code))
                        (when (> code #xFF) (fail))
                        (setf (deref buffer
                                     (the fixnum (+ i size)))
                              code)))))
          (setf (size bstream) needed)
          (succeed))))))

(defun serialize-to-utf16le (string bstream)
  "Serialize to utf16le compliant format unless contains code pages > 0"
  (declare (type buffer-stream bstream)
           (type string string))
  (with-slots (buffer size (allocated length)) bstream
    (let* ((saved-size (size bstream))
           (saved-pos (offset bstream))
           (characters (length string)))
      (labels ((fail () 
                 (setf (size bstream) saved-size)
                 (setf (offset bstream) saved-pos)
                 (return-from serialize-to-utf16le nil))
               (succeed ()
                 (return-from serialize-to-utf16le t)))
        (buffer-write-byte +utf16-string+ bstream)
        (buffer-write-int32 characters bstream)
        (let ((needed (+ size (* characters 2)))
              (char (etypecase string
                      (simple-string #'schar)
                      (string #'char))))
          (when (> needed allocated)
            (resize-buffer-stream bstream needed))
          (loop for i fixnum from 0 below characters do
                   (let ((code (char-code (funcall char string i))))
                     (when (> code #xFFFF) (fail))
                     (setf (deref buffer (+ (* i 2) size))
                           ;;			  (coerce (ldb (byte 8 8) code) '(signed 8)))
                           (ldb (byte 8 8) code))
                     (setf (deref buffer (+ (* i 2) size 1))
                           ;;			  (coerce (ldb (byte 8 0) code) '(signed 8))))))
                           (ldb (byte 8 0) code))))
          (incf size (* characters 2))
          (succeed))))))

(defun serialize-to-utf32le (string bstream)
  "Serialize to utf32 compliant format unless contains code pages > 0"
  (declare (type buffer-stream bstream)
           (type string string))
  (with-slots ((buffer buffer-stream-buffer)
               (size buffer-stream-size)
               (allocated buffer-stream-length))
      bstream
    (let ((characters (length string)))
      (buffer-write-byte +utf32-string+ bstream)
      (buffer-write-int32 characters bstream)
      (let ((needed (+ size (* 4 characters)))
            (char (etypecase string
                    (simple-string #'schar)
                    (string #'char))))
        (when (> needed allocated)
          (resize-buffer-stream bstream needed))
        (loop for i fixnum from 0 below characters do
                 (let ((code (char-code (funcall char string i))))
                   (when (> code #x10FFFF) (error "Invalid unicode code type"))
                   (setf (deref buffer (+ (* i 4) size 0))
                         (ldb (byte 8 24) code))
                   (setf (deref buffer (+ (* i 4) size 1))
                         (ldb (byte 8 16) code))
                   (setf (deref buffer (+ (* i 4) size 2))
                         (ldb (byte 8 8) code))
                   (setf (deref buffer (+ (* i 4) size 3))
                         (ldb (byte 8 0) code)))))
      (incf size (* characters 4))
      t)))

(defparameter *native-string-type*
  #+(and sbcl sb-unicode) :utf-32le
  #+(and sbcl (not sb-unicode)) :utf-8)

(defun compatible-unicode-support-p (encoding-type)
  "This is a crude hack and can be improved later, but
we assume if you have code pages > 0 you need or use a 32-bit encoding. We
assume that 16-bit unicode supporting lisps only support code page 0 and do
not use conjugate pair coding and variable length unicode string
representations (formal utf-16)."
  (or (eq encoding-type :utf-8) 
      (eq encoding-type *native-string-type*)
      (and (eq encoding-type :utf-16le) (eq *native-string-type* :utf-32le))))

(defun deserialize-string (bstream &key temp-string (type :utf-8))
  (declare (type buffer-stream bstream)
           (type (or null string) temp-string)
           (type symbol type))
  (let ((length (the fixnum (buffer-read-fixnum bstream)))
        (pos (the fixnum (offset bstream))))
    (case type
      (:utf-8 
       (incf (offset bstream) length)
       (utf8-decode-from-sap (buffer bstream)))
      (:utf-16le
       (let ((string (or temp-string (make-string length :element-type 'character)))
             (code 0))
         (macrolet ((next-byte (offset)
                      `(deref (buffer bstream) (+ (* i 2) pos ,offset))))
           (declare (type simple-string string)
                    (type fixnum length pos code))
           (assert (subtypep (type-of string) 'simple-string))
           (assert (compatible-unicode-support-p :utf-16le))
           (loop for i fixnum from 0 below length 
                 do (setf code (dpb (next-byte 0) (byte 8 8) 0))
                    (setf code (dpb (next-byte 1) (byte 8 0) code))
                    (setf (schar string i) (code-char code)))
           (incf (offset bstream)
             (* length 2)))
         (the simple-string string)))
      (:utf-32le
       (macrolet ((next-byte (offset)
                    `(deref (buffer bstream) (+ (* i 4) pos ,offset))))
         (let ((string (or temp-string (make-string length :element-type 'character)))
                (code 0))
      (declare (type string string)
               (type fixnum length pos code))
      (assert (subtypep (type-of string) 'simple-string))
      (assert (compatible-unicode-support-p :utf-32le))
      (loop for i fixnum from 0 below length do
               (setf code (dpb (next-byte 0) (byte 8 24) 0))
               (setf code (dpb (next-byte 1) (byte 8 16) code))
               (setf code (dpb (next-byte 2) (byte 8 8) code))
               (setf code (dpb (next-byte 3) (byte 8 0) code))
               (setf (char string i) (code-char code)))
      (incf (offset bstream)
        (* length 4))
      (the simple-string string)))))))

;; Serializer
;; store-object?
(defun serialize-object (frob bs sc)
  "Serialize a lisp value into a buffer-stream."
  (declare (type buffer-stream bs)
           (ignorable sc))
  (let ((lisp-obj-id -1)
        (circularity-hash 
          (unless (or (stringp frob) (symbolp frob) (numberp frob))
            (get-circularity-hash))))
    (declare (type fixnum lisp-obj-id))
    (labels 
        ((%next-object-id ()
           (incf lisp-obj-id))
         (%serialize (frob)
           ;;	   (format t "Serializing ~A of type ~A~%" frob (type-of frob))
           (typecase frob
             (fixnum 
              #+32-bit
              (progn
                (buffer-write-byte +fixnum32+ bs)
                (buffer-write-fixnum32 frob bs))
              #+64-bit
              (progn
                ;; (assert (eq (< #.most-positive-fixnum +2^63+) t))
                (if (< (abs frob) +2^31+)
                    (progn
                      (buffer-write-byte +fixnum32+ bs)
                      (buffer-write-fixnum32 frob bs))
                    (progn
                      (buffer-write-byte +fixnum64+ bs)
                      (buffer-write-fixnum64 frob bs)))))
             (null (buffer-write-byte +nil+ bs))
             (symbol
              (let ((sym-name (symbol-name frob)))
                (declare (type string sym-name)
                         (dynamic-extent sym-name))
                (buffer-write-byte +symbol+ bs)
                (serialize-string sym-name bs)
                (let ((package (symbol-package frob)))
                  (declare (dynamic-extent package)
                           (type (or null package) package))
                  (if package
                      (serialize-string (package-name package) bs)
                      (buffer-write-byte +nil+ bs)))))
             ;;		(let ((package-name (gethash frob symbol-package-hash)))
             ;;		  (unless package-name
             ;;		    (setq package-name 
             ;;			  (setf (gethash frob symbol-package-hash)
             ;;				(package-name (symbol-package frob)))))
             ;;		  (if package-name
             ;;		      (serialize-string package-name bs)
             ;;		      (buffer-write-byte +nil+ bs)))))
             (string
              (serialize-string frob bs))
             (stored
              (unless (valid-stored-reference-p frob sc)
                (signal-cross-store-error frob sc))
              #+todo
              (when (store-marking-p sc)
                (gc-mark-new-write frob))
              (buffer-write-byte +stored-ref+ bs)
              (buffer-write-oid (oid frob) bs))
             #+lispworks ; TODO: how does this affect performance?
             (short-float
              (buffer-write-byte +short-float+ bs)
              (buffer-write-float (coerce frob 'single-float) bs))
             (single-float
              (buffer-write-byte +single-float+ bs)
              (buffer-write-float frob bs))
             (double-float
              (buffer-write-byte +double-float+ bs)
              (buffer-write-double frob bs))
             (standard-object
              ;; NOTE: Add support for schema validation
              (buffer-write-byte +object+ bs)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (buffer-write-int32 idp bs)
                    (progn
                      (let ((id (%next-object-id)))
                        (buffer-write-int32 id bs)
                        (setf (gethash frob circularity-hash) id))
                      (%serialize (type-of frob))
                      (let ((svs (slots-and-values frob)))
                        (%serialize (/ (length svs) 2))
                        (loop for item in svs
                              do (%serialize item)))))))
             (integer
              (serialize-bignum frob bs))
             (rational
              (buffer-write-byte +rational+ bs)
              (%serialize (numerator frob))
              (%serialize (denominator frob)))
             (character
              (buffer-write-byte +char+ bs)
              ;; might be wide!
              (buffer-write-uint32 (char-code frob) bs))
             ;;	     (oid-pair
             ;;	      (buffer-write-byte +oid-pair+ bs)
             ;;	      (buffer-write-int32 (oid-pair-left frob) bs)
             ;;	      (buffer-write-int32 (oid-pair-right frob) bs))
             (cons
              (buffer-write-byte +cons+ bs)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (buffer-write-int32 idp bs)
                    (progn
                      (let ((id (%next-object-id)))
                        (buffer-write-int32 id bs)
                        (setf (gethash frob circularity-hash) id))
                      (%serialize (car frob))
                      (%serialize (cdr frob))))))
             (pathname
              (let ((pstring (namestring frob)))
                (buffer-write-byte +pathname+ bs)
                (serialize-string pstring bs)))
             (complex 
              (buffer-write-byte +complex+ bs)
              (%serialize (realpart frob))
              (%serialize (imagpart frob)))
             (hash-table
              (buffer-write-byte +hash-table+ bs)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (buffer-write-int32 idp bs)
                    (progn
                      (let ((id (%next-object-id)))
                        (buffer-write-int32 id bs)
                        (setf (gethash frob circularity-hash) id))
                      (%serialize (hash-table-test frob))
                      (%serialize (hash-table-rehash-size frob))
                      (%serialize (hash-table-rehash-threshold frob))
                      (%serialize (hash-table-count frob))
                      (loop for key being the hash-key of frob
                            using (hash-value value)
                            do 
                               (%serialize key)
                               (%serialize value))))))
             (array
              (buffer-write-byte +array+ bs)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (buffer-write-int32 idp bs)
                    (progn
                      (let ((id (%next-object-id)))
                        (buffer-write-int32 id bs)
                        (setf (gethash frob circularity-hash) id))
                      (buffer-write-byte 
                       (logior (octet-from-element-type (array-element-type frob))
                               (if (array-has-fill-pointer-p frob) 
                                   +fill-pointer-p+ 0)
                               (if (adjustable-array-p frob) 
                                   +adjustable-p+ 0))
                       bs)
                      (let ((rank (array-rank frob)))
                        (buffer-write-int32 rank bs)
                        (loop for i fixnum from 0 below rank
                              do (%serialize (array-dimension frob i))))
                      (when (array-has-fill-pointer-p frob)
                        (%serialize (fill-pointer frob)))
                      (loop for i fixnum from 0 below (array-total-size frob)
                            do
                               (%serialize (row-major-aref frob i)))))))
             (structure-object 
              (buffer-write-byte +struct+ bs)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (buffer-write-int32 idp bs)
                    (progn
                      (buffer-write-int32 (incf lisp-obj-id) bs)
                      (setf (gethash frob circularity-hash) lisp-obj-id)
                      (%serialize (type-of frob))
                      (let ((svs (struct-slots-and-values frob)))
                        (%serialize (/ (length svs) 2))
                        (loop for item in svs
                              do (%serialize item)))))))
             (t (format t "Can't serialize a object: ~A of type ~A~%" frob (type-of frob))))))
      (%serialize frob)
      (when circularity-hash
        (release-circularity-hash circularity-hash))
      bs)))

(defun serialize-bignum (frob bs)
  "Serialize bignum to buffer stream"
  (declare (type integer frob)
           (type buffer-stream bs))
  (let* ((num (abs frob))
         (word-size (ceiling (/ (integer-length num) 32)))
         (needed (* word-size 4))
         (byte-spec (byte 32 0)))
    (declare (type fixnum word-size needed)
             (type cons byte-spec)
             (ignorable byte-spec))
    (if (< frob 0) 
        (buffer-write-byte +negative-bignum+ bs)
        (buffer-write-byte +positive-bignum+ bs))
    (buffer-write-uint32 needed bs)
    (loop for i fixnum from 0 below word-size 
          ;; this ldb is consing on CMUCL/OpenMCL!
          ;; there is an OpenMCL function which should work 
          ;; and non-cons
          do
             #+cmu
             (buffer-write-uint32 (%bignum-ref num i) bs) ;; should fail under 64-bit CMU
             #-cmu
             (buffer-write-uint32 (ldb (byte 32 (* 32 i)) num) bs)
          )))

;; Deserializer

#+nil
(defparameter *trace-deserializer* t)

#+nil
(defparameter *tag-table*
  `((,+fixnum32+ . "fixnum32")
    (,+fixnum64+ . "fixnum64")
    (,+char+ . "char")
    (,+short-float+ . "short-float")
    (,+single-float+ . "single-float")
    (,+double-float+ . "double float")
    (,+negative-bignum+ . "neg bignum")
    (,+positive-bignum+ . "pos bignum")
    (,+rational+ . "rational number")
    (,+nil+ . "null")
    (,+utf8-string+ . "UTF8 string")
    (,+utf16-string+ . "UTF16le string")
    (,+utf32-string+ . "UTF32le string")
    (,+symbol+ . "symbol")
    (,+pathname+ . "pathname")
    (,+stored+ . "persistent object (old)")
    (,+stored-ref+ . "persistent object reference (new)")
;;    (,+oid-pair+ . "oid pair for associations")
    (,+cons+ . "cons cell")
    (,+hash-table+ . "hash table")
    (,+object+ . "standard object")
    (,+array+ . "array")
    (,+struct+ . "struct")
    (,+class+ . "class")
    (,+complex+ . "complex")))

#+nil
(defun enable-deserializer-tracing ()
  (setf *trace-deserializer* t))

#+nil
(defun disable-deserializer-tracing ()
  (setf *trace-deserializer* nil))

#+nil
(defun print-pre-deserialize-tag (tag)
  (when *trace-deserializer*
    (let ((tag-name (assoc tag *tag-table*)))
      (if tag-name
      (format t "Deserializing type: ~A~%" tag-name)
      (progn
        (format t "Unrecognized tag: ~A~%" tag)
        (break))))))

#+nil
(defun print-post-deserialize-value (value)
  (when *trace-deserializer*
    (format t "Returned: ~A~%" value)))

(define-condition store-deserialization-error (deserializer-error wrapped-error)
  ()
  (:documentation "A generalized deserialization error; something went wrong in deserialization
that an application can test for explicitely. The enclosed condition is the
actual error.")
  (:report store-deserialization-error-report))

(defun store-deserialization-error-report (condition stream)
  (format stream "Elephant deserialization error:~%~A"
          (wrapped-condition-value condition)))

(define-condition store-type-deserialization-error (store-deserialization-error)
  ((tag :initarg :type-tag :initform nil :accessor type-deserialization-error-tag))
  (:documentation "This error is signaled when a tag is not
                   recognized in the deserializer.  This error is
                   more specific than the generalized error
                   condition and aids in diagnosis.  This may be
                   due to a mistake in counts in the serializer
                   or a corruption of the source data"))

(defun deserialize (buf-str sc &optional oid-only)
  "Deserialize a lisp value from a buffer-stream."
  (declare (type (or null buffer-stream) buf-str))
  (let ((circularity-vector (get-circularity-vector)))
    (labels 
      ((lookup-id (id)
     (if (>= id (fill-pointer circularity-vector)) nil
         (aref circularity-vector id)))
       (add-object (object)
     (vector-push-extend object circularity-vector 50)
     (1- (fill-pointer circularity-vector)))
       (%deserialize (bs)
     (declare (type buffer-stream bs))
     (let ((tag (buffer-read-byte bs)))
       (declare (type (alien char) tag)
            (dynamic-extent tag))
;;	   (print-pre-deserialize-tag tag)
       (let ((value  
       (cond
         ((= tag +fixnum32+)
          (buffer-read-fixnum32 bs))
         ((= tag +fixnum64+)
          (buffer-read-fixnum64 bs))
         ((= tag +nil+) nil)
         ((= tag +utf8-string+)
          (deserialize-string bs :type :utf-8))
         ((= tag +utf16-string+)
          (deserialize-string bs :type :utf-16le))
         ((= tag +utf32-string+)
          (deserialize-string bs :type :utf-32le))
         ((= tag +symbol+)
          (let ((name (%deserialize bs))
            (package (%deserialize bs)))
        (translate-and-intern-symbol sc name package)))
         ((= tag +stored+)
          (let ((oid (buffer-read-oid bs))
            (cname (%deserialize bs)))
        (if oid-only oid
            (store-recreate-instance sc oid cname))))
         ((= tag +stored-ref+)
          (let ((oid (buffer-read-oid bs)))
        (if oid-only oid
            (store-recreate-instance sc oid))))
         #+lispworks
         ((= tag +short-float+)
          (coerce (buffer-read-float bs) 'short-float))
         ((= tag +single-float+)
          (buffer-read-float bs))
         ((= tag +double-float+)
          (buffer-read-double bs))
         ((= tag +char+)
          (code-char (buffer-read-uint32 bs)))
         ((= tag +pathname+)
          (parse-namestring (or (%deserialize bs) "")))
         ((= tag +positive-bignum+) 
          (deserialize-bignum bs (buffer-read-uint32 bs) t))
         ((= tag +negative-bignum+) 
          (deserialize-bignum bs (buffer-read-uint32 bs) nil))
         ((= tag +rational+) 
          (/ (the integer (%deserialize bs)) 
         (the integer (%deserialize bs))))
;;	     ((= tag +oid-pair+)
;;	      (let ((pair (make-oid-pair)))
;;		(setf (oid-pair-left pair) (buffer-read-fixnum32 bs))
;;		(setf (oid-pair-right pair) (buffer-read-fixnum32 bs))))
         ((= tag +cons+)
          (let* ((id (buffer-read-int32 bs))
             (maybe-cons (lookup-id id)))
        (declare (type fixnum id))
        (if maybe-cons maybe-cons
            (let ((c (cons nil nil)))
              (add-object c)
              (setf (car c) (%deserialize bs))
              (setf (cdr c) (%deserialize bs))
              c))))
         ((= tag +complex+)
          (let ((rpart (%deserialize bs))
            (ipart (%deserialize bs)))
        (complex rpart ipart)))
         ((= tag +hash-table+)
          (let* ((id (buffer-read-int32 bs))
             (maybe-hash (lookup-id id)))
        (declare (type fixnum id))
;;		(format t "~A ~A~%" maybe-hash id)
        (if maybe-hash maybe-hash
            (let* ((test (%deserialize bs))
               (rehash-size (%deserialize bs))
               (rehash-threshold (%deserialize bs))
               (size (%deserialize bs))
               (h (make-hash-table :test test
                           :rehash-size rehash-size
                           :rehash-threshold rehash-threshold
                           :size (ceiling (* (ceiling (/ (+ size 10) rehash-threshold)) rehash-size)))))
              (add-object h)
              (loop for i fixnum from 0 below size
                do
                (setf (gethash (%deserialize bs) h)
                  (%deserialize bs)))
              h))))
         ((= tag +object+)
          (let* ((id (buffer-read-int32 bs))
             (maybe-o (lookup-id id)))
        (if maybe-o maybe-o
            (let ((typedesig (%deserialize bs)))
              ;; now, depending on what typedesig is, we might 
              ;; or might not need to specify the store controller here..
              (let ((o 
                 (or (handler-case
                   (if (subtypep typedesig 'stored)
                       (recreate-instance-using-class (find-class typedesig) :sc sc)
                       ;; if the this type doesn't exist in our object
                       ;; space, we can't reconstitute it, but we don't want 
                       ;; to abort completely, we will return a special object...
                       ;; This behavior could be configurable; the user might 
                       ;; prefer an abort here, but I prefer surviving...
                       (make-instance typedesig))
                   (error (v) (format t "got typedesig error: ~A ~A ~%" v typedesig)
                      (list 'caught-error v typedesig)))
                 (list 'uninstantiable-object-of-type typedesig))))
            (if (listp o)
                o
                (progn
                  (add-object o)
                  (loop for i fixnum from 0 below (%deserialize bs)
                    do
                    (setf (slot-value o (%deserialize bs))
                      (%deserialize bs)))
                  o)))))))
         ((= tag +array+)
          (let* ((id (buffer-read-int32 bs))
             (maybe-array (lookup-id id)))
        (if maybe-array maybe-array
            (let* ((flags (buffer-read-byte bs))
               (a (make-array 
                   (loop for i fixnum from 0 below 
                     (buffer-read-int32 bs)
                     collect (%deserialize bs))
                   :element-type (element-type-from-octet
                          (logand #x1f flags))
                   :fill-pointer (/= 0 (logand +fill-pointer-p+ 
                               flags))
                   :adjustable (/= 0 (logand +adjustable-p+ 
                             flags)))))
              (when (array-has-fill-pointer-p a)
            (setf (fill-pointer a) (%deserialize bs)))
              (add-object a)
              (loop for i fixnum from 0 below (array-total-size a)
                do
                (setf (row-major-aref a i) (%deserialize bs)))
              a))))
         ((= tag +struct+)
          (let* ((id (buffer-read-int32 bs))
             (maybe-o (lookup-id id)))
        (if maybe-o maybe-o
            (let ((typedesig (%deserialize bs)))
              (let ((o (or (handler-case
                       (funcall (struct-constructor typedesig))
                     (error (v) (format t "got typedesig error for struct: ~A ~A ~%" v typedesig)
                        (list 'caught-error v typedesig)))
                   (list 'uninstantiable-object-of-type typedesig))))
            (if (listp o) o
                (progn
                  (add-object o)
                  (loop for i fixnum from 0 below (%deserialize bs) do
                   (let ((name (%deserialize bs))
                     (value (%deserialize bs)))
                     (setf (slot-value o name) value)))
                  o)))))))
         (t (error 'stored-type-deserialization-error :type-tag tag)))))
;;	     (print-post-deserialize-value value)
         value))))
      (etypecase buf-str 
    (null (return-from deserialize nil))
    (buffer-stream
     (let ((result (%deserialize buf-str)))
       (release-circularity-vector circularity-vector)
       result))))))

(defun int-byte-spec (position)
  "Shared byte-spec peformance hack; not thread safe.."
  (declare (type (unsigned-byte 24) position))
  (byte 32 (* 32 position)))

(defun deserialize-bignum (bs length positive)
  (declare (type buffer-stream bs)
           (type fixnum length)
           (type boolean positive))
  (let ((int-byte-spec (byte 32 0)))
    (declare (dynamic-extent int-byte-spec)
         (ignorable int-byte-spec))
    (loop for i from 0 below (/ length 4)
       for byte-spec = (byte 32 (* 32 i))
          with num of-type integer = 0 
          do
             (setq num (dpb (buffer-read-uint32 bs) byte-spec num))
          finally 
             (return (if positive num (- num))))))

;;; Macros
#+nil
(defmacro defstore (name super spec &rest options)
  "Define a new STORE class.")

#+nil
(defmacro with-store ((sym &rest initargs &key &allow-other-keys) &body body)
  "Similar to WITH-DB but for STORE objects instead of DATABASEs. 

INITARGS are passed to OPEN-STORE.")

;;; HACK: re-implementation of SB-MOP internals (compute-slots)
;; this will require benchmarking to determine if we need to lock this behind
;; an initialization function.
(in-package :SB-PCL)

(declaim (sb-ext:disable-package-locks sb-mop:compute-slots 
                                       sb-mop:class-slots
                                       sb-pcl::update-slots))

(defmethod find-nonstandard-slot-definition-location ((allocation (eql :database)) slot)
  (declare (ignore slot))
  nil)

(defmethod compute-slots :around ((class standard-class))
  (loop with slotds = (call-next-method) and location = -1
        for slot in slotds 
        for allocation = (slot-definition-allocation slot) 
        do (progn
             (setf (slot-definition-location slot)
                   (case allocation
                     (:instance
                      (incf location))
                     (:class
                         (let* ((name (slot-definition-name slot))
                                (from-class (sb-pcl::slot-definition-allocation-class slot))
                                (cell (sb-int:assq name (sb-pcl::class-slot-cells from-class))))
                           (assert (consp cell))
                           cell))
                     (t
                      (find-nonstandard-slot-definition-location allocation slot)))) 
             (sb-pcl::initialize-internal-slot-functions slot))	
        finally (return slotds)))
