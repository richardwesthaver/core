;;; store.lisp --- Data Store Protocols

;; Support for Lisp Stores.

;;; Commentary:

;; Inspired by Elephant

;; STOREs differ from DBs in that they always prefer transactions over simple
;; set/get.

;;; Code:
(defpackage :obj/store
  (:nicknames :store)
  (:use :cl :std :stored :sb-mop :meta)
  (:export
   #:register-instance
   #:cache-instance
   #:get-cached-instance
   #:uncache-instance
   #:flush-instance-cache
   #:stored-slot-reader
   #:stored-slot-writer
   #:stored-slot-boundp
   #:stored-slot-makunbound
   #:store
   #:make-cache-table
   #:next-oid
   #:next-cid
   #:drop-instance
   #:get-store
   #:*store*))

(in-package :obj/store)

(defvar *store* nil)

(defgeneric get-store (self))
(defgeneric (setf get-store) (new self))

(defgeneric stored-slot-reader (sc instance name &optional oids-only)
  (:documentation 
   "Store-specific slot reader function"))

(defgeneric stored-slot-writer (sc new-value instance name)
  (:documentation 
   "Store-specific slot writer function"))

(defgeneric stored-slot-boundp (sc instance name)
  (:documentation
   "Store-specific slot bound test function"))

(defgeneric stored-slot-makunbound (sc instance name)
  (:documentation
   "Store-specific slot makunbound handler"))

(defgeneric register-instance (self class intance))
(defgeneric cache-instance (self obj))
(defgeneric get-cached-instance (self oid))
(defgeneric uncache-instance (self oid))
(defgeneric flush-instance-cache (self))

(defgeneric next-oid (sc)
  (:documentation
   "The source of unique object IDs."))

(defgeneric next-cid (sc)
  (:documentation
   "The source of unique class schema IDs."))

(defclass store () 
  ((spec :type list
         :accessor spec
         :initarg :spec
         :documentation "Data store initialization functions are
         expected to initialize :spec on the call to
         make-instance")
   (schema-table :reader schema-table
                 :documentation "Schema id to schema database table")
   (schema-name-index :reader schema-name-index
                      :documentation "Schema name to schema database table")
   (schema-cache :accessor schema-cache :initform (make-cache-table :test 'eq)
                 :documentation "This is a cache of class schemas stored in the database indexed by classid")
   (schema-classes :accessor schema-classes :initform nil
                      :documentation "Maintains a list of all classes that have a cached schema value so we can shutdown cleanly")
   (schema-cache-lock :accessor schema-cache-lock :initform (sb-concurrency:make-frlock :name "cache-lock")
                        :documentation "Protection for updates to the cache from multiple threads.  
                                        Do not override.")
   ;; Instance storage
   (instance-table :reader instance-table
                  :documentation "Contains btree of oid to class ids")
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
               also a data store specific persistent btree instance
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
  "Each persistent instance has an oid and a home controller spec"
  (declare (ignore initargs))
  (initial-stored-setup instance :oid oid :store store))

(defun initial-stored-setup (instance &key oid store)
  (assert store)
  (if oid
      (setf (oid instance) oid)
      (register-new-instance instance (class-of instance) store))
  (setf (spec instance) (spec store))
  (cache-instance store instance))

(defun register-new-instance (instance class store)
  (setf (oid instance) (next-oid store))
  (register-instance store class instance))

(defun check-valid-store (store)
  (if-let ((ok (subtypep (type-of store) 'store)))
    ok
    (error "This function requires a valid store controller")))

(defgeneric drop-instance (persistent-object)
  (:documentation   "drop-instance reclaims persistent object storage by unbinding
   all persistent slot values. It can also helps catch errors where an object
   should be unreachable, but a reference still exists elsewhere in the DB. On
   access, the unbound slots should flag an error in the application
   program. IMPORTANT: this function does not clear any serialized references
   still in the db.  Need a migration or GC for that!  drop-instances is the
   user-facing call as it implements the proper behavior for indexed classes."))

(defmethod drop-instance ((inst stored-object))
  (let ((sc (get-store inst)))
    (ensure-transaction (:store sc)
      (drop-instance-slots inst)
      (call-next-method))))

(defmethod drop-instance ((inst stored))
  (let ((sc (get-store inst)))
    (with-mutex ((instance-cache-lock sc))
      (remcache (oid inst) (instance-cache sc)))
    (remove-kv (oid inst) (instance-table sc))))

(defun drop-instance-slots (instance)
  "A helper function for drop-instance, that deletes the storage of 
   persistent slots of instance from the db"
  (let ((class (class-of instance)))
    (loop for slot-def in (class-slots class)
       when (stored-p slot-def)
       do (slot-makunbound-using-class class instance slot-def))))

(defun dropped-instance-p (sc oid)
  "An instance has not been dropped if it is in the instance
   table and has a valid class id"
  (multiple-value-bind (cid found?)
      (get-value oid (controller-instance-table sc))
    (and cid found?)))

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
    (ensure-transaction (:store sc)
      (let* ((idx (get-slot-def-index slot-def sc))
             (old-value-bound-p (slot-boundp-using-class class instance slot-def))
             (old-value (when old-value-bound-p
                          (slot-value-using-class class instance slot-def))))
        (unless idx
          (setf idx (ensure-slot-def-index slot-def sc)))
        (when old-value-bound-p 
          (remove-kv-pair old-value oid idx))
        (setf (get-value new-value idx) oid)))))

(defun get-store-index (slot-def sc)
  "Get the slot-def's index from the store"
  (let* ((master (index-table sc))
         (base (indexed-slot-base slot-def))
         (name (slot-definition-name slot-def)))
    (get-value (cons base name) master)))

(defun ensure-slot-def-index (slot-def sc)
  "If a slot's index does not exist, create it"
  (aif (get-controller-index slot-def sc)
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
  (remove-kv (cons class-name index-name) (index-table sc)))

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

;; open-store
;; close-store
;; close-all-stores
;; with-open-store
;; with-store
;; drop-instances

;;; Cache
(defun make-cache-table (&rest args)
  "Make a value-weak hashtable. When value gets collected so does the key."
  (apply 'make-hash-table :weakness :value args))

(defun get-cache (key cache)
  "Get a value from a cache-table."
  (let ((val (gethash key cache)))
    (if val (values (sb-ext:weak-pointer-value val) t)
        (values nil nil))))

(defun make-finalizer (key cache)
  (declare (ignorable key cache))
  (lambda () (remhash key cache)))

(defun remcache (key cache)
  (remhash key cache))

(defun setf-cache (key cache value)
  "Set a value in a cache-table."
  (let ((w (sb-ext:make-weak-pointer value)))
    (sb-ext:finalize value (make-finalizer key cache))
    (setf (gethash key cache) w)
    value))

(defsetf get-cache setf-cache)

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

;;; Slot Access
(defmethod slot-value-using-class ((class stored-class) (instance stored-object) (slot-def stored-slot-definition))
  "Get the slot value from the database."
  (let ((name (slot-definition-name slot-def)))
    (stored-slot-reader (get-store instance) instance name)))

(defmethod (setf slot-value-using-class) (new-value (class stored-class) (instance stored-object) (slot-def stored-slot-definition))
  "Set the slot value in the database."
  (let ((name (slot-definition-name slot-def)))
    (ensure-transaction (:store (get-store instance))
      (cond
        ((derived-slot-triggers slot-def)
         (stored-slot-writer (get-store instance) new-value instance name)
         (derived-index-updater class instance slot-def))
        (t (stored-slot-writer (get-store instance) new-value instance name)))))
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
