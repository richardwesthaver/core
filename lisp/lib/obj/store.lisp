;;; store.lisp --- Data Store Protocols

;; Support for Lisp Stores.

;;; Commentary:

;; Inspired by Elephant

;; STOREs differ from DBs in that they always prefer transactions over simple
;; set/get.

;;; Code:
(defpackage :obj/store
  (:nicknames :store)
  (:use :cl :std :stored :sb-mop :meta :btree :id :db :schema)
  (:export
   #:store
   #:make-cache-table
   #:next-oid
   #:next-cid
   #:*store*
   #:spec
   #:temp-spec
   #:delete-spec
   #:copy-spec
   #:optimize-layout
   #:oid->schema-id
   #:default-class-id
   #:default-class-id-type
   #:reserved-oid-p
   #:add-class-store-schema
   #:dropped-instance-p
   #:drop-instance-slots
   #:drop-instance
   #:store-recreate-instance
   #:recreate-instance
   #:recreate-instance-using-class
   #:valid-stored-reference-p
   #:cross-store-error
   #:signal-cross-store-error
   #:with-store
   #:defstore))

(in-package :obj/store)

(defvar *store* nil)

(defvar *stores* nil)

(defun make-btree (&optional (st *store*))
  "Constructs a new BTree instance for use by the user.  Each backend
   returns its own internal type as appropriate and ensures that the 
   btree is associated with the store-controller that created it."
  (build-btree st))

(defun make-indexed-btree (&optional (sc *store*))
  "Constructs a new indexed BTree instance for use by the user.
   Each backend returns its own internal type as appropriate and
   ensures that the btree is associated with the store-controller
   that created it."
  (build-indexed-btree sc))

(defun make-dup-btree (&optional (st *store*))
  (btree::build-dup-btree st))

(defgeneric next-oid (store)
  (:documentation
   "The source of unique object IDs."))

(defgeneric next-cid (store)
  (:documentation
   "The source of unique class schema IDs."))

(defun unindex-slot-value (sc key value old-name old-base)
  (let* ((master (index-table sc))
         (index (get-value (cons old-base old-name) master)))
    (remove-kv key value index)))

;;; Schema
(defclass stored-object-schema (object-schema database-schema) ())

(defmethod print-object ((schema stored-object-schema) stream)
  (print-unreadable-object (schema stream :type t)
    (format stream "~A ~A (s: ~A p: ~A)" 
            (id schema) (schema:schema-class-name schema)
            (schema:schema-successor schema) (schema:schema-predecessor schema))))

(defun make-stored-object-schema (cid class-schema)
  (let ((schema (logical-copy-schema 'stored-object-schema class-schema)))
    (setf (id schema) cid)
    schema))

(defun logical-copy-schema (type schema)
  (assert (subtypep type 'schema:schema))
  (make-instance type
    :class-name (schema:schema-class-name schema)
    :fields (copy-list (schema:fields schema))))

(defun copy-schema (type schema)
  (assert (subtypep type 'schema:schema))
  (let ((new 
         (make-instance type
                        :name (schema:schema-class-name schema)
                        :successor (schema:schema-successor schema)
                        :predecessor (schema:schema-predecessor schema)
                        :fields (copy-array (schema:fields schema)))))
    (when (subtypep (type-of schema) 'database-schema)
      (setf (id new) (id schema))
      (setf (upgrade new) (upgrade schema))
      (setf (version new) (version schema)))
    new))

;;; DB Evolution
(defmethod upgrade-db-instance ((instance stored:stored-object) (new-schema database-schema) (old-schema database-schema) old-values)
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
            ((and (member old-type '(:persistent :cached))
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
            ;; If it was a persistent slot and now isn't, drop it and add the new type back
            ((and (member old-type '(:persistent :indexed :cached :derived))
                  (not (member (slot-field-type new-rec) '(:persistent :indexed :cached :derived))))
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
    (when (member type '(:persistent :cached :indexed :derived))
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
  (labels ((adding-persistent? (entry)
             (when (and (eq :add (diff-type entry))
                        (member (slot-field-type (first (diff-recs entry)))
                                '(:persistent :indexed :cached :set-valued)))
               (slot-field-name (first (diff-recs entry)))))
           (change-to-persistent? (entry)
             (when (and (eq :change (diff-type entry))
                        (not (member (slot-field-type (first (diff-recs entry)))
                                     '(:persistent :indexed :cached :set-valued)))
                        (member (slot-field-type (second (diff-recs entry)))
                                '(:persistent :indexed :cached :set-valued)))
               (slot-field-name (second (diff-recs entry)))))
           (init-slot? (entry)
             (or (adding-persistent? entry)
                 (change-to-persistent? entry)))
           (compute-init-slots ()
             (remove-if #'null (mapcar #'init-slot? diff))))
    (apply #'shared-initialize instance (compute-init-slots) nil)))

(defmethod change-db-instance ((current stored-object) previous
                               new-schema old-schema)
  "Change a database instance from one schema & class to another
   These are different objects with the same oid"
  (let ((sc (get-store current))
        (oid (oid current))
        (diff (schema-diff new-schema old-schema)))
      ;; do we need to pass the persistent object?  Transient ops require previous?
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
;;   (dump-btree (instance-table sc))
;;   (dump-index (index-table sc))
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

(defgeneric temp-spec (type spec))
(defgeneric delete-spec (type spec))
(defgeneric copy-spec (type src dst))

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
  (:method recreate-instance ((instance stored-collection) &rest initargs &key oid (st *store*))
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
        #-openmcl
        (let ((prior-schema (aif (schema:schema-predecessor current-schema)
                                 (get-store-schema st it)
                                 (error "If the schemas mismatch, a derived store schema should have been computed"))))
          (assert (and current-schema prior-schema))
          (upgrade-db-instance instance current-schema prior-schema property-list)))))

(defmethod change-class :before ((previous stored) (new-class standard-class) &rest initargs)
  (declare (ignorable initargs))
  (unless (subtypep (type-of new-class) 'stored-class)
    (error "Stored instances cannot be changed to standard classes via change-class")))

(defmethod update-instance-for-different-class :after ((previous stored-object) (current stored-object) 
                                                        &rest initargs &key)
  ;; Update db to new class configuration
  ;; - handle indices, removals, associations and additions
  (let* ((sc (get-store current))
         (current-schema (lookup-schema sc (class-of current)))
         (previous-schema (lookup-schema sc (class-of previous))))
    (assert (eq sc (get-store previous)))
    (change-db-instance current previous current-schema previous-schema)
    ;; Deal with new persistent slot, cached and transient initialization
    (let* ((diff-entries (schema-diff current-schema previous-schema))
           (add-entries (remove-if-not (lambda (entry) (eq :add (diff-type entry))) diff-entries))
           (add-names (when add-entries (mapcar #'field-name (mapcan #'diff-recs add-entries)))))
      (apply #'shared-initialize current add-names initargs))))

(defmethod change-class :before ((previous standard-object) (new-class stored-class) &rest initargs)
  (declare (ignorable initargs)) 
  (unless (subtypep (type-of previous) 'stored)
    (error "Cannot convert standard objects to stored objects")))

;;; Store
(defclass store () 
  ((spec :type list
         :accessor spec
         :initarg :spec
         :documentation "Data store initialization functions are
         expected to initialize :spec on the call to
         make-instance")
   ;; Generic support for the object, indexing and root protocols
   (root :reader store-root 
         :documentation "This is an instance of the data store
         btree.  It should have an OID that is fixed in the code and does not
         change between sessions.  Usually it this is something like 0, 1 or
         -1")
   (schema-table :reader schema-table
                 :documentation "Schema id to schema database table")
   (schema-name-index :reader schema-name-index
                      :documentation "Schema name to schema database table")
   (schema-cache :accessor schema-cache :initform (make-cache-table :test 'eq)
                 :documentation "This is a cache of class schemas stored in the database indexed by classid")
   (schema-classes :accessor schema-classes :initform nil
                      :documentation "Maintains a list of all classes that have a cached schema value so we can shutdown cleanly")
   (schema-cache-lock :accessor schema-cache-lock :initform (make-mutex :name "cache-lock")
                        :documentation "Protection for updates to the cache from multiple threads.  
                                        Do not override.")
   ;; Instance storage
   (instance-table :reader instance-table
                  :documentation "Contains map of oid to class ids")
   (instance-class-index :reader instance-class-index
                         :documentation "A reverse map of class id to oid")
   (instance-cache :accessor instance-cache :initform (make-cache-table :test 'eql)
                   :documentation 
                   "This is an instance cache and part of the
                    metaclass protocol.  Data stores should not
                    override the default behavior.")
   (instance-cache-lock :accessor instance-cache-lock :initform (make-mutex :name "instance-cache")
                        :documentation "Protection for updates to
                        the cache from multiple threads.  Do not
                        override.")
   ;; Root table for all indices
   (index-table :reader index-table
               :documentation 
               "This is another root for class indexing that is
               also a data store specific stored btree instance
               with a unique OID that persists between sessions.
               No cache is needed because we cache in the class slots.")
   (serializer :accessor serializer :initform nil)
   (deserializer :accessor deserializer :initform nil)))

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
  (assert store)
  (if oid
      (setf (oid instance) oid)
      (register-new-instance instance (class-of instance) store))
  (setf (spec instance) (spec store))
  (cache-instance store instance))

(defun class-schema-id (st class)
  (if (subtypep (class-name class) 'btree)
      (default-class-id (class-name class) st)
      (id (lookup-schema st class))))

(defmethod register-instance ((st store) cl instance)
  (set-instance-schema-id st (oid instance) (class-schema-id st cl)))  

(defmethod set-instance-schema-id ((st store) oid cid)
  (let ((table (instance-table st)))
    (delete-key oid table)
    (setf (get-value oid table) cid)))

(defmethod get-instance-class ((st store) oid &optional classname)
  "Get the class object using the oid or using the provided classname"
  (when classname
    (return-from get-instance-class (find-class classname)))
  (let ((cid (oid->schema-id oid st)))
    (unless cid
      (signal-missing-instance oid (spec st))
      (return-from get-instance-class (find-class 'stored-object)))
    (get-schema-id-class st cid)))

(defmethod get-schema-id-class ((st store) cid)
  "Get the class given the schema id"
  (aif (default-class-id-type cid st)
       (find-class it)
       (let ((schema (get-store-schema st cid)))
         (values (find-class (schema-class-name schema)) schema))))

(define-condition missing-stored-instance (simple-condition)
   ((oid :initarg :oid :accessor error-oid)
    (spec :initarg :spec :accessor error-spec)))

(defun signal-missing-instance (oid spec)
  (cerror "Return a proxy object"
          'missing-stored-instance
          :format-control "Instance with OID ~A is not stored in ~A"
          :format-arguments (list oid spec)
          :oid oid
          :spec spec))

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

(defun register-new-instance (instance class store)
  (setf (oid instance) (next-oid store))
  (register-instance store class instance))

(defun check-valid-store (store)
  (if-let ((ok (subtypep (type-of store) 'store)))
    ok
    (error "This function requires a valid store")))

(defmethod drop-instance ((inst stored-object))
  (drop-instance-slots inst)
  (call-next-method))

(defmethod drop-instance ((inst stored))
  (let ((sc (get-store inst)))
    (with-mutex ((instance-cache-lock sc))
      (remcache (oid inst) (instance-cache sc)))
    (delete-key (oid inst) (instance-table sc))))

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
      (get-value oid (instance-table st))
    (and cid found?)))

(defmethod oid->schema-id (oid (st store))
  (get-value oid (instance-table st)))

(defgeneric default-class-id (base-type sc)
  (:documentation "A method implemented by the store for providing
   fixed class ids for basic btree derivative types"))

(defgeneric default-class-id-type (id sc)
  (:documentation "A method implemented by the store which provides
   the type associated with a default id or nil if the id does not match"))

(defgeneric reserved-oid-p (sc oid)
  (:documentation "Is this OID reserved by the store? GC doesn't touch"))

(defmethod add-class-store-schema (st (class stored-class) schema)
  ;; NOTE: Needs to be lock protected
  (pushnew (class-name class) (schema-classes st))
  (remove-class-store-schema st class)
  (setf (get-store-schemas class)
        (acons (spec st) schema (get-store-schemas class))))

(defmethod remove-class-store-schema (st (class stored-class))
  ;; NOTE: Needs to be lock protected
  (setf (get-store-schemas class)
        (remove (spec st) (get-store-schemas class) 
                :key #'car :test #'equalp)))

(defmethod get-class-store-schema (st (class stored-class))
  (awhen (assoc (spec st) (get-store-schemas class))
    (cdr it)))

;;; Cache
(defun make-cache-table (&rest args)
  "Make a value-weak hashtable. When value gets collected so does the key."
  (apply 'make-hash-table :weakness :value args))

(defun get-cache (key cache)
  "Get a value from a cache-table."
  (let ((val (gethash key cache)))
    (if val (values (sb-ext:weak-pointer-value val) t)
        (values nil nil))))

(defsetf get-cache setf-cache)

(defun setf-cache (key cache value)
  "Set a value in a cache-table."
  (let ((w (sb-ext:make-weak-pointer value)))
    (sb-ext:finalize value (make-finalizer key cache))
    (setf (gethash key cache) w)
    value))

(defun make-finalizer (key cache)
  (declare (ignorable key cache))
  (lambda () (remhash key cache)))

(defun remcache (key cache)
  (remhash key cache))

(defun map-cache (fn cache)
  (with-hash-table-iterator (nextfn cache)
    (loop  
       (multiple-value-bind (valid? key value) (nextfn)
         (when (not valid?)
           (return-from map-cache))
         (funcall fn key (sb-ext:weak-pointer-value value))))))

(defun dump-cache (cache)
  (format t "Dumping cache: ~A~%" cache)
  (map-cache #'(lambda (k v) 
                 (format t "key: ~A / value: ~A~%" k v))
             cache))

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
          (remcache schema-id (schema-cache st)))
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
  (let* ((master (index-table sc))
         (base (indexed-slot-base slot-def))
         (name (slot-definition-name slot-def)))
    (get-value (cons base name) master)))

(defun ensure-slot-def-index (slot-def sc)
  "If a slot's index does not exist, create it"
  (aif (get-store-index slot-def sc)
       (progn (add-slot-def-index it slot-def sc) it)
       (let ((new-idx (make-dup-btree sc)))
         (add-slot-index sc new-idx (indexed-slot-base slot-def) (slot-definition-name slot-def))
         (add-slot-def-index new-idx slot-def sc)
         new-idx)))

(defmethod add-slot-index ((sc store) new-index class-name index-name)
  "Add it to the index table and the class slot def"
  (setf (get-value (cons class-name index-name) (index-table sc))
        new-index))

(defmethod drop-slot-index ((sc store) class-name index-name)
  (clear-slot-def-index (find-slot-def-by-name (find-class class-name) index-name) sc)
  (delete-key (cons class-name index-name) (index-table sc)))

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
                      (instance-class-index sc)
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
    (btree::with-btree-cursor (cur index)
      (multiple-value-bind (valid? value oid)
          (btree::cursor-first cur)
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
    (btree::with-btree-cursor (cur btree)
      (multiple-value-bind (valid k) (btree::cursor-next cur)
        (declare (ignore k))
        (cond ((not valid) ;; truly empty
               t)
              ((eq btree (store-root (get-store btree)))
               (not (btree::cursor-next cur)))
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

;;; Controller Protocol
(defgeneric open-store (st &key recover recover-fatal thread &allow-other-keys)
  (:documentation ""))

(defgeneric close-store (st))

(defgeneric optimize-layout (st &key &allow-other-keys))

;;; Controller User API

;; start stop

(defun close-all-stores ()
  (loop for pair in *stores*
       do (close-store (cdr pair))))

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
  "Add an arbitrary persistent thing to the root, so you can
   retrieve it in a later session.  Anything referenced by an
   object added to the root is considered reachable and thus live"
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

(defmacro defstore (name super spec &rest options))

(defmacro with-store (sym &body body))
