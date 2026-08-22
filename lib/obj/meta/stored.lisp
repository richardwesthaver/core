;;; obj/meta/stored.lisp --- CLOS Stored Metaclasses

;; The stored-class can be assigned to the :metaclass option of a
;; class to allow persistent storage of an object on disk. The
;; stored-slot-definition is a custom slot option which can be used to
;; selectively enable slot serialization.

;;; Commentary:

;; This code is derived from XDB.

;; Note that this is not a general purpose SerDe. It is specifically designed
;; to decode/encode objects as simple octet-vectors from/to an open stream
;; with minimal overhead. There is a separate interface for general-purpose
;; data encoding which can be found in the DAT system.

;;; Code:
(in-package :obj/meta/stored)

(defvar *default-store* (cons nil nil))
(defvar *store* nil)
;; support for swapping out multiple stores? compatibility matrix?
(defvar *store-table* (make-hash-table))

;;; MOP
(defclass stored ()
  ((oid :initarg :oid :accessor oid)
   (spec :accessor spec :initarg :spec
         :documentation "Stored objects use a spec pointer to identify which store
                         they are connected to"))
  (:documentation "Slots which are implicitly bound to all STORED-CLASS metaobjects."))

(defmethod print-object ((obj stored) stream)
  "This is useful for debugging and being clear about what is stored and what is
not."
  (format stream "#<~A oid:~A>" (type-of obj) (when (slot-boundp obj 'oid) (oid obj))))

(defun write-oid (i bs) (write-sequence (integer-to-octets i 32) bs))

(defun read-oid (bs) 
  (octets-to-integer
   (coerce 
    'octet-vector
    (loop for i below 4
          collect (read-byte bs)))))

(defclass stored-collection (stored) ()
  (:documentation "Abstract superclass of all STORED collection types."))

(defgeneric drop-instance (stored-object)
  (:documentation   "drop-instance reclaims stored object storage by unbinding
   all stored slot values. It can also helps catch errors where an object
   should be unreachable, but a reference still exists elsewhere in the DB. On
   access, the unbound slots should flag an error in the application
   program. IMPORTANT: this function does not clear any serialized references
   still in the db.  Need a migration or GC for that!  drop-instances is the
   user-facing call as it implements the proper behavior for indexed classes."))

(defgeneric get-store (self)  
  (:documentation "Get the store associated with SELF. We prefix this accessor with GET- because
STORE is reserved for a special method which operates on stored objects.")
  (:method ((self t))
    *store*))

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

(defgeneric register-instance (self class instance))
(defgeneric cache-instance (self obj))
(defgeneric get-cached-instance (self oid))
(defgeneric uncache-instance (self oid))
(defgeneric flush-instance-cache (self))

(defclass stored-class (standard-class)
  ((%class-schema :accessor %class-schema :initarg :schemas :initform nil)
   (%store-schemas :accessor %store-schemas :initarg :store-schemas :initform nil)
   (%class-indexing :accessor %class-indexing :initarg :index :initform t)
   (%cache-style :accessor %cache-style :initarg :cache-style :initform nil))
  (:documentation "Superclass for all stored classes."))

(defmethod get-class-schema (self) (slot-value self '%class-schema))
(defmethod set-class-schema (self value)
  (setf (slot-value self '%class-schema) value))
(defsetf get-class-schema set-class-schema)

(defmethod get-store-schemas (self) (slot-value self '%store-schemas))
(defmethod set-store-schemas (self value) 
  (setf (slot-value self '%store-schemas) value))
(defsetf get-store-schemas set-store-schemas)

(defmethod get-class-indexing (self) (slot-value self '%class-indexing))
(defsetf get-class-indexing (self) (value)
  `(setf (slot-value ,self '%class-indexing) ,value))

(defmethod get-cache-style (self) (slot-value self '%cache-style))
(defsetf get-cache-style (self) (value)
  `(setf (slot-value ,self '%cache-style) ,value))

(defmethod class-indexing-enabled-p ((class stored-class))
  (and (not (subtypep (class-name class) 'stored-collection))
       (get-class-indexing class)))

(defun migrate-class-index-p (class)
  (get-class-indexing class))

(defmethod find-slot-defs-by-type ((class stored-class) type &optional (by-subtype t))
  (let ((slot-defs (class-slots class)))
    (loop for slot-def in slot-defs
         when (if by-subtype
                  (subtypep (type-of slot-def) type)
                  (eq (type-of slot-def) type))
         collect slot-def)))

(defmethod find-slot-def-names-by-type ((class stored-class) type &optional (by-subtype t))
  (mapcar #'slot-definition-name 
          (find-slot-defs-by-type class type by-subtype)))

;;; Validate
(defmethod validate-superclass
    ((class standard-class)
     (superclass stored-class))
  nil)

(defmethod validate-superclass
    ((class stored-class)
     (superclass standard-class))
  t)

(defclass stored-object (stored) ()
  (:metaclass stored-class)
  (:documentation 
   "Superclass for all user-defined stored classes. This is
    automatically inherited if you use the STORED-CLASS
    metaclass."))

(defmethod shared-initialize :around ((class stored-class) slot-names &rest args &key direct-superclasses direct-slots index cache-style)
  "Ensures we inherit from stored-object prior to initializing."
  (declare (ignorable index))
  ;; When we declare slots and don't initialize the cache-style and don't
  ;; inherit cacheable-persistent-object...inform the user
  (when (and (not cache-style) (cached-slot-specification-p direct-slots)
             (not (superclass-member-p 'stored-cache-object (class-direct-superclasses class))))
    (error "Must specify the class caching style if you declare cached slots and don't~%inherit from a cached class.  Class option :cache-style must be one of~% :checkout, :txn or :none"))
  (let* ((new-direct-superclasses 
           (if cache-style 
               ;; TODO 2026-07-21: stored-cache-object
               (ensure-class-inherits-from class '(stored-cache-object) direct-superclasses)
               (ensure-class-inherits-from class '(stored-object) direct-superclasses))))
    ;; Call the next method
    (prog1
        (apply #'call-next-method class slot-names
               :direct-superclasses new-direct-superclasses 
               (remove-from-plist args :direct-superclasses :index))
      ;; Make sure we convert the cache argument so it can be used in accessors
      (when (consp (get-cache-style class))
        (setf (get-cache-style class) (first (get-cache-style class)))))))

(defun ensure-class-inherits-from (class from-classnames direct-superclasses)
  (let* ((from-classes (mapcar #'find-class from-classnames))
         (has-persistent-objects 
           (every #'(lambda (class) (superclass-member-p class direct-superclasses))
                  from-classes)))
    (if (not (or (member class from-classes) has-persistent-objects))
        (progn
          (dolist (class from-classes)
            (setf direct-superclasses (remove class direct-superclasses)))
          (append direct-superclasses from-classes))
        direct-superclasses)))

(defun superclass-member-p (class superclasses)
  "Searches superclass list for class"
  (some #'(lambda (superclass)
        (or (eq class superclass)
        (let ((supers (class-direct-superclasses superclass)))
          (when supers
            (superclass-member-p class supers)))))
    superclasses))

(defun cached-slot-specification-p (direct-slot-defs)
  (some #'(lambda (slot) (getf slot :cached)) direct-slot-defs))

;;; Slot mixin
(defclass stored-slot-definition (standard-slot-definition)
  ((stored-p :initarg :stored
           :initform t
           :accessor stored-p)))

(defgeneric stored-p (mclass)
  (:method ((mclass t)) nil)
  (:method ((mclass stored-class)) t)
  (:method ((mclass stored-slot-definition)) t))

(defclass stored-direct-slot-definition (stored-slot-definition standard-direct-slot-definition)
  ())

(defclass stored-effective-slot-definition (stored-slot-definition standard-effective-slot-definition)
  ((triggers :accessor derived-slot-triggers :initarg :trigger :initform nil)))

(defmethod direct-slot-definition-class ((class stored-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'stored-direct-slot-definition))

(defmethod effective-slot-definition-class ((class stored-class)
                                            &key &allow-other-keys)
  (find-class 'stored-effective-slot-definition))

(defmethod compute-effective-slot-definition
    ((class stored-class) slot-name direct-definitions)
  (declare (ignore slot-name))
  (let ((effective-definition (call-next-method))
        (direct-definition (car direct-definitions)))
    (setf (stored-p effective-definition)
          (stored-p direct-definition))
    effective-definition))

(defun make-slots-cache (slot-definitions)
  (map 'vector
       (lambda (slot-definition)
	 (cons (slot-definition-location slot-definition)
	       (slot-definition-initform slot-definition)))
       slot-definitions))

(defun stored-slot-defs (class)
  (find-slot-defs-by-type class 'stored-effective-slot-definition nil))

(defun stored-slot-names (class)
  (find-slot-def-names-by-type class 'stored-effective-slot-definition nil))

(defun all-stored-slot-names (class)
  (append (find-slot-def-names-by-type class 'stored-effective-slot-definition t)
          (find-slot-def-names-by-type class 'cached-effective-slot-definition t)))

(defun all-single-valued-slot-defs (class)
  (append (stored-slot-defs class)
          (cached-slot-defs class)
          (indexed-slot-defs class)))

(defun compute-derived-index-triggers (class derived-slot-defs)
  (let* ((sdefs (all-single-valued-slot-defs class))
     (sdef-names (mapcar #'slot-definition-name sdefs)))
    (dolist (ddef derived-slot-defs)
      (dolist (sname (ifret (derived-slot-deps ddef) sdef-names))
    (unless (member sname sdef-names)
      (error "Finalization error: derived index dependency hint ~A not a valid slot name"
         sname))
    (push ddef (derived-slot-triggers (find-slot-def-by-name class sname)))))))

;;; From Elephant - for future development
(defclass cached-slot-definition (standard-slot-definition)
  ((cache :accessor cached-slot-p :initarg :cached)))

(defclass cached-direct-slot-definition (standard-direct-slot-definition cached-slot-definition)
  ())

(defclass cached-effective-slot-definition (standard-effective-slot-definition cached-slot-definition)
  ((triggers :accessor derived-slot-triggers :initarg :trigger :initform nil)))

(defun cached-slot-defs (class)
  (find-slot-defs-by-type class 'cached-effective-slot-definition nil))

(defun cached-slot-names (class)
  (find-slot-def-names-by-type class 'cached-effective-slot-definition nil))

;;; Transient Slots
(defclass transient-slot-definition (standard-slot-definition)
  ((transient :initform t :initarg :transient :allocation :class)))

(defclass transient-direct-slot-definition (standard-direct-slot-definition transient-slot-definition)
  ())

(defclass transient-effective-slot-definition (standard-effective-slot-definition transient-slot-definition)
  ())

(defgeneric transient-p (slot)
  (:method ((slot standard-slot-definition)) t)
  (:method ((slot transient-slot-definition)) t)
  (:method ((slot cached-slot-definition)) nil)
  (:method ((slot stored-slot-definition)) nil))

(defun ensure-transient-chain (slot-definitions initargs)
  (declare (ignore initargs))
  (loop for slot-definition in slot-definitions
     always (transient-p slot-definition)))

(defun transient-slot-defs (class)
  (let ((slot-definitions (class-slots class)))
    (loop for slot-def in slot-definitions
       when (transient-p slot-def)
       collect slot-def)))

(defun transient-slot-names (class)
  (mapcar #'slot-definition-name (transient-slot-defs class)))

(defgeneric database-allocation-p (class)
  (:method ((class t)) nil)
  (:method ((class stored-class)) t)
  (:method ((class stored-slot-definition)) t))

(defmethod slot-definition-allocation ((slot-definition stored-slot-definition))
  :database)

;;; Indexed Slots
(defclass indexed-slot-definition (stored-slot-definition)
  ((indexed :accessor indexed-p :initarg :indexed :initarg :index :initform nil :allocation :instance)
   (inherit :accessor inherit-p :initarg :inherit :initform nil :allocation :instance)))

(defclass indexed-direct-slot-definition (stored-direct-slot-definition indexed-slot-definition)
  ())

(defclass indexed-effective-slot-definition (stored-effective-slot-definition indexed-slot-definition)
  ((indices :accessor indexed-slot-indices :initform nil :allocation :instance
            :documentation "Alist of actual indices by store")
   (base-class :accessor indexed-slot-base :initarg :base-class :allocation :instance
               :documentation "The base class to use as an index")))

(defmethod indexed-p (def)
  (declare (ignore def))
  nil)

(defmethod get-slot-def-index ((def indexed-effective-slot-definition) sc)
  (awhen (assoc sc (indexed-slot-indices def))
    (cdr it)))

(defmethod add-slot-def-index (idx (def indexed-effective-slot-definition) sc)
  (setf (indexed-slot-indices def)
        (acons sc idx (indexed-slot-indices def))))

(defmethod clear-slot-def-index ((def indexed-effective-slot-definition) sc)
  (setf (indexed-slot-indices def)
        (remove sc (indexed-slot-indices def) :key #'car)))

(defmethod indexed-slot-defs (class)
  (find-slot-def-names-by-type class 'indexed-effective-slot-definition nil))

(defmethod indexed-slot-names (class)
  (find-slot-def-names-by-type class 'indexed-effective-slot-definition nil))

(defclass derived-index-slot-definition (indexed-slot-definition)
  ((derived-fn-ref :accessor derived-fn-ref :initarg :derived-fn)
   (slot-deps :accessor derived-slot-deps :initarg :slot-deps :initarg :slot-dependencies :initform nil)))

(defclass derived-index-direct-slot-definition (indexed-direct-slot-definition derived-index-slot-definition)
  ())

(defclass derived-index-effective-slot-definition (indexed-effective-slot-definition derived-index-slot-definition)
  ((fn :accessor derived-fn :initarg :fn)))

(defmethod derived-index-slot-defs (class)
  (find-slot-defs-by-type class 'derived-index-effective-slot-definition nil))

(defmethod derived-index-slot-names (class)
  (find-slot-def-names-by-type class 'derived-index-effective-slot-definition nil))

(defun compile-derived-fn (ref)
  (if (symbolp ref)
      (handler-case 
          (and (functionp (symbol-function ref))
               (gen-derived-fn-wrapper (compile ref)))
        (undefined-function (ref) (error "~A does not appear to be a valid function reference" ref)))
      (if (listp ref)
          (gen-derived-fn-wrapper (compile nil (eval ref)))
          (error "~A does not appear to be a valid function expression" ref))))

(defun gen-derived-sym-wrapper (symbol-fn)
  "Return a closure to handle errors in the derived index function"
  (lambda (inst)
    (handler-case 
        (funcall (symbol-function symbol-fn) inst)
      (unbound-slot ()
        (values nil nil))
      (error (e)
        (cerror "Ignoring?"
                "error ~A while computing derived value for ~A" 
                e inst)
        (values nil nil)))))

(defun gen-derived-fn-wrapper (compiled)
  "Return a closure to handle errors in the derived index function"
  (lambda (inst)
    (handler-case 
        (funcall compiled inst)
      (unbound-slot ()
        (values nil nil))
      (error (e)
        (cerror "Ignoring?"
                "error ~A while computing derived value for ~A" 
                e inst)
        (values nil nil)))))

(defclass set-valued-slot-definition (stored-slot-definition) 
  ((set-valued-p :accessor set-valued-p :initarg :set-valued :allocation :instance)))

(defclass set-valued-direct-slot-definition (stored-direct-slot-definition set-valued-slot-definition) 
  ())

(defclass set-valued-effective-slot-definition (stored-effective-slot-definition set-valued-slot-definition) 
  ())

(defun set-valued-slot-defs (class)
  (find-slot-defs-by-type class 'set-valued-effective-slot-definition nil))

(defun set-valued-slot-names (class)
  (find-slot-def-names-by-type class 'set-valued-effective-slot-definition nil))

(defclass association-slot-definition (stored-slot-definition)
  ((assoc :accessor association :initarg :associate :allocation :instance)
   (inherit :accessor inherit-p :initarg :inherit :initform nil :allocation :instance)
   (m2m :accessor many-to-many-p :initarg :many-to-many :initform nil :allocation :instance)))

(defclass association-direct-slot-definition (stored-direct-slot-definition association-slot-definition) 
  ())

(defclass association-effective-slot-definition (stored-effective-slot-definition association-slot-definition) 
  ((type :accessor association-type :initarg :association-type)
   (base-class :accessor association-slot-base :initarg :base-class :allocation :instance
               :documentation "The base class to use as an index")
   (indices :accessor association-slot-indices :initform nil 
            :documentation "Alist of actual indices by store")
   (classname :accessor foreign-classname :initarg :foreign-classname)
   (slotname :accessor foreign-slotname :initarg :foreign-slotname)
   (class :accessor foreign-class :initarg :foreign-class :initform nil
          :documentation "Direct pointer to foreign class; late binding")))

(defmethod initialize-instance :after ((slot-def association-effective-slot-definition) &rest args)
  (declare (ignore args))
  (let ((assoc (association slot-def)))
    (cond ((symbolp assoc)
           (when (many-to-many-p slot-def)
             (error "Cannot specify ~A in a many-to-many association, must be of form (class slotname)"
                    assoc))
           (setf (association-type slot-def) :ref
                 (foreign-classname slot-def) assoc
                 (foreign-slotname slot-def) nil))
          (t 
           (destructuring-bind (classname slotname) assoc
             (setf (foreign-classname slot-def) classname)
             (setf (foreign-slotname slot-def) slotname)
             (if (many-to-many-p slot-def)
                 (setf (association-type slot-def) :m2m)
                 (setf (association-type slot-def) :m21)))))))

(defun association-end-p (slot-def)
  (not (eq (association-type slot-def) :m21)))

(defun association-slot-defs (class)
  (find-slot-defs-by-type class 'association-effective-slot-definition nil))

(defun association-slot-names (class)
  (find-slot-def-names-by-type class 'association-effective-slot-definition nil))

(defun association-end-slot-names (class)
  (let ((results nil))
    (mapc #'(lambda (slot-def)
              (when (association-end-p slot-def)
                (push (slot-definition-name slot-def) results)))
          (find-slot-defs-by-type class 'association-effective-slot-definition nil))
    results))

(defun get-association-slot-index (slot-def sc)
  (awhen (assoc sc (association-slot-indices slot-def))
    (cdr it)))

(defun add-association-slot-index (idx slot-def sc)
  (setf (association-slot-indices slot-def)
        (acons sc idx (association-slot-indices slot-def))))

(defun remove-association-slot-index (slot-def sc)
  (setf (association-slot-indices slot-def)
        (delete sc (association-slot-indices slot-def) :key #'car)))

(defmacro bind-standard-init-arguments ((initargs) &body body)
  `(let ((allocation-key (getf ,initargs :allocation))
         (has-initarg-p (getf ,initargs :initargs))
         (transient-p (getf ,initargs :transient))
         (indexed-p (or (getf ,initargs :indexed)
                        (getf ,initargs :index)))
         (derived-p (or (getf ,initargs :derived-fn)
                        (getf ,initargs :fn)))
         (cached-p (getf ,initargs :cached))
         (set-valued-p (getf ,initargs :set-valued))
         (associate-p (getf ,initargs :associate)))
     (declare (ignorable allocation-key has-initarg-p))
     (when (consp transient-p) (setq transient-p (car transient-p)))
     (when (consp indexed-p) (setq indexed-p (car indexed-p)))
     (when (consp derived-p) (setq derived-p (car derived-p)))
     (when (consp cached-p) (setq cached-p (car cached-p)))
     (when (consp set-valued-p) (setq set-valued-p (car set-valued-p)))
     (when (consp associate-p) (setq associate-p (car associate-p)))
     ,@body))

(defmethod direct-slot-definition-class ((class stored-class) &rest initargs)
  "Checks for the transient tag (and the allocation type)
   and chooses stored or transient slot definitions."
  (bind-standard-init-arguments (initargs)
    (cond ((and (eq allocation-key :class) (not transient-p))
           (error "Stored class slots are not supported, try :transient t."))
          ((> (count t (list (or indexed-p derived-p) transient-p)) 1)
           (error "Cannot declare a slot to be more than one of transient or indexed."))
          ((and set-valued-p has-initarg-p)
           (error "Cannot specify initargs for set-valued slots"))
          ((and associate-p (or (not (member (type-of associate-p) '(cons symbol))) (eq associate-p t)))
           (error "':associate' slot initarg must contain classname or a class / slot reference: (classname slotname)"))
          ((and associate-p has-initarg-p (eq (type-of associate-p) 'cons))
           (error "Can only specify initargs for association slots storing single instances of another class"))
          (derived-p
           (find-class 'derived-index-direct-slot-definition))
          (indexed-p
           (find-class 'indexed-direct-slot-definition))
          (set-valued-p
           (find-class 'set-valued-direct-slot-definition))
          (cached-p
           (find-class 'cached-direct-slot-definition))
          (associate-p
           (find-class 'association-direct-slot-definition))
          (transient-p
           (find-class 'transient-direct-slot-definition))
          (t
           (find-class 'stored-direct-slot-definition)))))

(defmethod effective-slot-definition-class ((class stored-class) &rest initargs)
  "Chooses the stored or transient effective slot
definition class depending on the keyword."
  (bind-standard-init-arguments (initargs)
    (cond (derived-p
           (find-class 'derived-index-effective-slot-definition))
          (indexed-p 
           (find-class 'indexed-effective-slot-definition))
          (set-valued-p
           (find-class 'set-valued-effective-slot-definition))
          (cached-p
           (find-class 'cached-effective-slot-definition))
          (associate-p
           (find-class 'association-effective-slot-definition))
          (transient-p
           (find-class 'transient-effective-slot-definition))
          (t
           (find-class 'stored-effective-slot-definition)))))

(defmethod compute-effective-slot-definition-initargs ((class stored-class) slot-definitions)
  (let ((initargs (call-next-method))
        (parent-direct-slot (first slot-definitions)))
    (cond ((ensure-transient-chain slot-definitions initargs)
           (setf initargs (append initargs '(:transient t))))
          ((not (eq (type-of parent-direct-slot) 'cached-direct-slot-definition))
           (setf (getf initargs :allocation) :database)))
    (when (eq (type-of parent-direct-slot) 'set-valued-direct-slot-definition)
      (setf (getf initargs :set-valued) t))
    (when (eq (type-of parent-direct-slot) 'cached-direct-slot-definition)
      (setf (getf initargs :cached) t))
    (when (eq (type-of parent-direct-slot) 'association-direct-slot-definition)
      (setf (getf initargs :associate) (association parent-direct-slot))
      (setf (getf initargs :inherit)
            (inherit-p parent-direct-slot))
      (setf (getf initargs :many-to-many) (many-to-many-p parent-direct-slot))
      (setf (getf initargs :base-class)
            (if (inherit-p parent-direct-slot)
                (find-class-for-direct-slot class parent-direct-slot)
                (class-name class))))
    (when (eq (type-of parent-direct-slot) 'indexed-direct-slot-definition)
      (setf (getf initargs :indexed) t)
      (setf (getf initargs :inherit) 
            (inherit-p parent-direct-slot))
      (setf (getf initargs :base-class)
            (if (inherit-p parent-direct-slot)
                (find-class-for-direct-slot class parent-direct-slot)
                (class-name class))))
    (when (eq (type-of parent-direct-slot) 'derived-index-direct-slot-definition)
      (setf (getf initargs :derived-fn)
            (derived-fn-ref parent-direct-slot))
      (setf (getf initargs :inherit) 
            (inherit-p parent-direct-slot))
      (setf (getf initargs :slot-deps)
            (derived-slot-deps parent-direct-slot))
      (setf (getf initargs :fn)
            (compile-derived-fn (derived-fn-ref parent-direct-slot)))
      (setf (getf initargs :base-class)
            (if (inherit-p parent-direct-slot)
                (find-class-for-direct-slot class parent-direct-slot)
                (class-name class))))
    initargs))

(defun find-class-for-direct-slot (class def)
  (let ((list (compute-class-precedence-list class)))
    (labels ((rec (super)
               (if (null super)
                   nil
                   (aif (find-direct-slot-def-by-name super (slot-definition-name def))
                        (class-name super)
                        (rec (pop list))))))
      (rec class))))

(defmethod change-class :before ((previous standard-object) (new-class stored-class) &rest initargs)
  (declare (ignorable initargs)) 
  (unless (subtypep (type-of previous) 'stored)
    (error "Cannot convert standard objects to stored objects")))

(defmethod change-class :before ((previous stored) (new-class standard-class) &rest initargs)
  (declare (ignorable initargs))
  (unless (subtypep (type-of new-class) 'stored-class)
    (error "Stored instances cannot be changed to standard classes via change-class")))

;;; Macros
(defmacro defsclass (cname parents slot-defs &rest class-opts)
  "Shorthand for defining stored objects.  Wraps the main
   class definition with stored-class"
  `(eval-always
     (defclass ,cname ,parents
       ,slot-defs
       ,@(add-stored-metaclass-argument class-opts))))

(defun add-stored-metaclass-argument (class-opts)
  (when (assoc :metaclass class-opts)
    (error "User metaclass specification not allowed in defsclass"))
  (append class-opts (list (list :metaclass 'stored-class))))
