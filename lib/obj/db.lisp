;;; lib/obj/db/proto.lisp --- Database Protocol

;;

;;; Commentary:

;; This set of 

;;; Code:
(in-package :obj/db)

;;; Vars
(defvar *db* nil)
(defvar *database-backend* nil)
(defvar *database-collection-type* 'list)
(defvar *default-database-version* '(0 1 0))
(defvar *default-kv-size* 8)
(defparameter *save-database-backend-on-load* nil)
;;; Backends
(defvar *database-backends* (make-hash-table)
  "Hash Table where keys are a database backend designator and values
are a list of functions which are responsible for doing all initialization
such as loading shared libraries and setting variables.")

(defvar *database-backend-options* (make-hash-table)
  "Hash Table where keys are a database backend designator and values are a
lambda-list which will be interpreted by PARSE-DATABASE-BACKEND-OPTIONS within
the body of WITH-DB forms.")

(defvar *database-backend-close-options* '(close destroy))

(defun add-database-loader (backend thunk)
  (let ((flist (gethash backend *database-backends*)))
    (setf (gethash backend *database-backends*) (pushnew thunk flist :test 'equalp))))

(defun add-database-backend-option (backend option)
  "Add a new database backend option."
  (let ((olist (gethash backend *database-backend-options*)))
    (setf (gethash backend *database-backend-options*) (pushnew option olist))))

(defun set-database-backend (backend options &rest thunks)
  "Set the loaders (a sequence of thunks) and options for the designated database
backend keyword BACKEND."
  (setf (gethash backend *database-backends*) thunks
        (gethash backend *database-backend-options*) options))

(declaim (inline %load-database-backend))
(defun %load-database-backend (backend)
  (when-let ((be (gethash backend *database-backends*)))
    (dolist (th be)
      (funcall th))))

(defun load-database-backend (backend &optional save)
  "Load database BACKEND and set value of *DATABASE-BACKEND*. When SAVE is
non-nil also arrange for the BACKEND to be loaded on init when this core is
saved."
  (let ((*save-database-backend-on-load* save))
    (%load-database-backend backend)
    (setq *database-backend* backend)))

(defun %database-backend-option-key (item)
  (keywordicate (if (atom item) item (car item))))

;; TODO 2024-11-10: should we handle &rest/&optional too?
(defun parse-database-backend-options (initargs)
  "Parse INITARGS as a plist of database options for current *DATABASE-BACKEND*."
  (mapcar ;; for each registered database backend option..
   (lambda (opt)
     (let ((key (%database-backend-option-key opt)))
       (if (member key initargs)
           (let ((match (getf initargs key)))
             (if (atom opt) (cons opt match) (cons (car opt) match)))
           opt)))
   (gethash *database-backend* *database-backend-options*)))

(defgeneric set-database-backend-option (db key val)
  (:method (db (key (eql :open)) val)
    (when val
      (open-db db)))
  (:method (db (key (eql :close)) val)
    (when val
      (close-db db)))
  (:method (db (key (eql :destroy)) val)
    (when val
      (close-db db)
      (destroy-db db)))
  (:method (db (key (eql :path)) val)
    (setf (path db) val))
  (:method (db (key (eql :name)) val)
    (setf (name db) val))
  (:method (db (key (eql :id)) val)
    (setf (id db) val))
  (:method (db (key (eql :sap)) val)
    (setf (sap db) val))
  (:method (db (key (eql :opts)) val)
    (setf (db-opts db) val))
  (:method (db (key (eql :opt)) (val cons))
    (set-db-opt db (car val) (cdr val)))
  (:method (db (key (eql :shutdown)) val)
    (shutdown-db db :wait (eql val :wait))))

(defun set-database-backend-options (db &rest options)
  (mapc (lambda (opt)
          (set-database-backend-option
           db
           (keywordicate (car opt))
           ;; WARNING eval here
           (eval (cdr opt))))
        options))

(defun do-database-backend-init-options (db &rest options)
  (apply 'set-database-backend-options
         db
         (remove-if
          (lambda (x) 
            (or (atom x)
                (null (cdr x))
                (member (car x) *database-backend-close-options*)))
          options)))

(defun do-database-backend-close-options (db &rest options)
  (apply 'set-database-backend-options
         db
         (remove-if
          (lambda (x)
            (or (atom x)
                (null (cdr x))
                (not (member (car x) *database-backend-close-options*))))
          options)))

(defmacro with-db ((var &rest initargs &key db &allow-other-keys) 
                   &body body)
  "Bind VAR to a DATABASE instance produced by parsing INITARGS for the extent
  of BODY."
  `(let ((opts ',(parse-database-backend-options initargs))
         (,var (or ,db *db*)))
     ,@(when db `((setf *db* ,var)))
     ;; ,@(when open (remf initargs :open) `((open-db ,var)))
     (apply 'do-database-backend-init-options ,var opts)
     (unwind-protect (progn ,@body)
       ;; ,@(when close (remf initargs :close) `((close-db ,var)))
       ;; ,@(when destroy (remf initargs :destroy) `((destroy-db ,var)))
       (apply 'do-database-backend-close-options ,var opts))))

;;; Config
(defconfig db-config ()
  ((backend :initform :rdb :type database-backend-designator)
   (options)))

;;; Conditions
(define-condition db-condition () ()
  (:documentation "Superclass for DB conditions."))

(deferror not-a-database (db-condition invalid-argument) ()
  (:documentation "Error signaled when an illegal DB is detected.")
  (:default-initargs
   :reason "Object is not a database")
  (:auto t))

;;; Database
(defgeneric db (self)
  (:documentation "Return the Database associated with SELF."))

(defgeneric db-lock (self)
  (:documentation "Return an optional database MUTEX."))

(defgeneric database-version (self)
  (:documentation "Return the version associated with a given database SELF."))

(defmethod database-version :around (self)
  (declare (ignorable self))
  (let ((version (call-next-method)))
    (std/macs:ifret version
      *default-database-version*)))

(defun prior-version-p (v1 v2)
  "Is v1 an equal or earlier version than v2"
  (cond ((and (null v1) (null v2))         t)
        ((and (null v1) (not (null v2)))   t)
        ((and (not (null v1)) (null v2))   nil)
        ((< (car v1) (car v2))             t)
        ((> (car v1) (car v2))             nil)
        ((= (car v1) (car v2))
         (prior-version-p (cdr v1) (cdr v2)))
        (t (error "Version comparison problem: (prior-version-p ~A ~A)" v1 v2))))

(defclass database ()
  ((db :initform nil :initarg :db :accessor db))
  (:documentation "Base class for Database objects."))

(defclass upgradable-schema (schema)
  ((version :accessor version :initarg :version :initform 1)
   (upgrade :accessor upgrade :initform nil))
  (:documentation "A schema which may be upgraded in-place."))

(defmethod print-object ((self upgradable-schema) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A ~A" (id self) (version self))))

(defmethod dump-schema ((self upgradable-schema) &optional (stream t))
  (awhen (upgrade self)
    (format stream "upgrade:~%~A~%" it)))

(defun apply-schema-change-fn (instance expr old-schema)
  (cond ((functionp expr)
         (funcall expr instance))
        ((symbolp expr)
         (funcall (symbol-function expr) instance))
        ((consp expr)
         (let ((fn (compile nil (eval expr))))
           (setf (upgrade old-schema) fn)
           (funcall fn instance)))))

(defclass database-collection () ()
  (:documentation "A collection of DATABASE objects."))

(defgeneric make-db (engine &rest initargs &key &allow-other-keys)
  (:documentation "Dispatch initializer for databases. An ENGINE must be supplied, which is
usually a key such as :ROCKSDB or :SQLITE."))

(defgeneric connect-db (db &key &allow-other-keys)
  (:documentation "Connect the database DB."))

(defgeneric query-db (db query &key &allow-other-keys)
  (:documentation "Execute QUERY against DB."))

(defgeneric db-get (db key &key &allow-other-keys)
  (:documentation "Return the value associated with KEY from DB."))

(defgeneric (setf db-get) (db key val &key &allow-other-keys))

(defgeneric close-db (db &key &allow-other-keys)
  (:documentation "Close a database."))

(defgeneric open-db (self)
  (:documentation "Open a database."))

(defgeneric destroy-db (self)
  (:documentation "Destroy all traces of a database, deleting any on-disk data and shutting down
in-memory objects."))

(defgeneric find-db (dbs name &key &allow-other-keys)
  (:documentation "Return the db by NAME, from a collection of databases DBS."))

(defgeneric insert-db (dbs name &key &allow-other-keys)
  (:documentation "Inserts a database by NAME into the database-collection DBS."))

(defgeneric db-open-p (self)
  (:documentation "Return T when database SELF is open.")
  (:method ((self t)) (not-a-database self))
  (:method ((self database)) (when (db self) t)))

(defgeneric db-closed-p (self)
  (:documentation "Return T when database SELF is closed.")
  (:method ((self t)) (not-a-database self))
  (:method ((self database)) (unless (db self) t)))

;;; Common
(defun slot-val (instance slot-name)
  (if (and instance
           (slot-boundp instance slot-name))
      (slot-value instance slot-name)))

(defgeneric get-val (object element &key &allow-other-keys)
  (:documentation "Returns the value in a object based on the supplied element name and possible
type hints.")
  (:method (object element &key data-type)
    (when object
      (typecase object
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
                 (error "Does not handle this type of object. Implement your own get-val method."))))))))

(defgeneric (setf get-val) (new-value object element &key &allow-other-keys)
  (:documentation "Set the value in a object based on the supplied element name and possible type
hints.")
  (:method (new-value object element &key data-type)
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
               (error "Does not handle this type of object. Implement your own get-val method.")))))))

(defgeneric get-value (elt obj)
  (:method (elt (obj sequence))
    (find elt obj :test 'equal))
  (:method (elt (obj hash-table))
    (gethash elt obj)))

(defgeneric (setf get-value) (new elt obj))

(defgeneric put-kv (self kv)
  (:documentation "Insert a KeyVal object."))
(defgeneric put-key (self key val)
  (:documentation "Insert a KEY and VAL."))
(defgeneric put-key-ts (self key val ts)
  (:documentation "Insert a KEY and VAL with associated timestamp TS."))
(defgeneric get-key (self key &key)
  (:documentation "Get value of KEY."))
(defgeneric multi-get (self keys &key)
  (:documentation "Retrieve multiple KEYS from SELF."))

(defgeneric insert-key (self key val &key)
  (:documentation "Insert KEY:VAL into SELF."))
(defgeneric insert-kv (self kv &key)
  (:documentation "Insert KV object into SELF."))
(defgeneric delete-key (self key &key)
  (:documentation "Delete value associated with KEY from SELF."))
(defmethod remove-kv (key value self))
(defgeneric delete-key-ts (self key ts)
  (:documentation "Delete value associated with KEY and TS from SELF."))
(defgeneric delete-key-range (self start end &key)
  (:documentation "Delete values associates with keys between START and END from SELF."))
(defgeneric flush-db (self &key)
  (:documentation "Flush the database SELF."))
(defgeneric sync-db (self other &key) ;;nyi
  (:documentation "Perform a synchronization on SELF using OTHER."))
(defgeneric load-db (self)
  (:documentation "Load an existing database."))
(defgeneric db-stats (self &optional type)
  (:documentation "Return TYPE stats of given database."))
(defgeneric db-metadata (self &optional type)
  (:documentation "Return TYPE metdata of given database."))
(defgeneric db-prop (self type)
  (:documentation "Return TYPE property of given database."))
(defgeneric db-opt (self key)
  (:documentation "Return value of database option KEY."))
(defgeneric db-opts (self)
  (:documentation "Accessor for database options of SELF."))
(defgeneric (setf db-opts) (new self)
  (:documentation "Return value of database option KEY."))
(defgeneric (setf db-opt) (new self key &key &allow-other-keys)
  (:documentation "Set the value of database option KEY."))
(defgeneric set-db-opt (self key val &key &allow-other-keys)
  (:documentation "Convenience setter for DB-OPT.")
  (:method ((self t) key val &key)
    (setf (db-opt self key) val))
  (:method ((self t) key val &key push)
    (setf (db-opt self key :push push) val)))
(defgeneric repair-db (self &key)
  (:documentation "Attempt to repair the database SELF."))
(defgeneric backup-db (self &key)
  (:documentation "Create a new backup for database SELF."))
(defgeneric db-backup (self)
  (:documentation "Access the current backup of database SELF."))
(defgeneric secondary-db (self)
  (:documentation "Accessor for the secondary-db of a database SELF."))
(defgeneric restore-db (self from &key)
  (:documentation "Restore database SELF from object FROM."))
(defgeneric snapshot-db (self)
  (:documentation "Create a new snapshot for database SELF."))
(defgeneric write-batch (self batch &key)
  (:documentation "Write BATCH to database SELF."))
(defgeneric shutdown-db (self &key wait &allow-other-keys)
  (:documentation "Shutdown database SELF."))
(defgeneric ingest-into-db (self file &key)
  (:documentation "Ingest an external file into the database"))

;; Merge Ops
(defgeneric merge-key (self key val &key)
  (:documentation "Perform a merge operation on SELF using KEY and VAL."))
(defgeneric merge-kv (self kv &key)
  (:documentation "Perform a merge operation on SELF using object KV."))

(defmacro with-merge-op ())

;; Columns
(defgeneric open-column (self col &key)
  (:documentation "Open and return a column from SELF."))
(defgeneric open-columns (self &rest columns)
  (:documentation "Open the columns or all columns belonging to SELF."))
(defgeneric open-columns* (self)
  (:documentation "Open all columns belonging to SELF."))
(defgeneric open-with-columns (self &rest names)
  (:documentation "Open a database with columns indicated by NAMES or all columns belonging to
SELF. This function may error when (DB-OPEN-P SELF) is non-nil."))
(defgeneric close-column (self &optional error)
  (:documentation "Close the column SELF. When ERROR is non-nil signal an error if the
column is already closed."))
(defgeneric close-columns (self)
  (:documentation "Close the columns belonging to SELF."))
(defgeneric destroy-column (self &optional error)
  (:documentation "Close the column SELF. When ERROR is non-nil signal an error if the
column is already closed."))
(defgeneric destroy-columns (self)
  (:documentation "Close the columns belonging to SELF."))
(defgeneric create-column (self cf)
  (:documentation "Create the column belonging to SELF."))
(defgeneric create-columns (self)
  (:documentation "Create the columns belonging to SELF."))
(defgeneric find-column (col self &key)
  (:documentation "Find the column COL in SELF."))
(defgeneric (setf find-column) (new col self &key)
  (:documentation "Find the column COL in SELF."))
(defgeneric flush-column (self col &key)
  (:documentation "Flush the column COL in SELF."))
(defgeneric add-column (col self)
  (:documentation "Add a column to SELF."))
(defgeneric column-opts (col))
(defgeneric (setf column-opts) (new col))

;;; KV
(defstruct (kv (:constructor make-kv (&optional key val))) 
  (key (make-octets *default-kv-size*) :type octet-vector) 
  (val (make-octets *default-kv-size*) :type octet-vector))

(defgeneric make-val (val)
  (:documentation "Coerce VAL into an OCTET-VECTOR.")
  (:method ((val null))
    #())
  (:method ((val string))
    (sb-ext:string-to-octets val))
  (:method ((val vector))
    (if (octet-vector-p val)
        val
        (call-next-method)))
  (:method ((val t))
    (coerce val 'octet-vector)))

(defgeneric make-key (key)
  (:documentation "Coerce KEY into an OCTET-VECTOR.")
  (:method ((val null))
    #())
  (:method ((val string))
    (sb-ext:string-to-octets val))
  (:method ((val integer))
    (integer-to-octets val))
  (:method ((val vector))
    (if (octet-vector-p val)
        val
        (call-next-method)))
  (:method ((val t))
    (coerce val 'octet-vector)))

;;; Transactions

;; In our system, transactions must be one of the following:

;; - A non-nil list 
;; - A subclass of TRANSACTION-OBJECT
;; - Implement a TRANSACTION-DB method which returns an instance of DATABASE

;; Simple transactions are non-nil lists which are handled according to the
;; current database backend

;; 
(deftype simple-transaction () `(and (not null) list))

(defvar *default-txn* '(nil nil nil))
(defvar *txn* nil
  "The current transaction.")

(defclass transaction-object () ()
  (:documentation "Base class for transaction objects."))

(defgeneric (setf transaction-opts) (new txn))
(defgeneric make-transaction (self &key)
  (:documentation "Make a new transaction object.")
  (:method ((self null) &key) *default-txn*))

(defgeneric prepare-transaction (self &key)
  (:documentation "Prepare a transaction."))
(defgeneric rollback-transaction (self &key)
  (:documentation "Rollback a transaction."))
(defgeneric commit-transaction (self &key)
  (:documentation "Commit a transaction."))
(defgeneric execute-transaction (self txn &rest args &key &allow-other-keys))
(defgeneric abort-transaction (self &key &allow-other-keys))

(defgeneric transaction-object-p (self)
  (:method ((self t))
    (or (typep 'simple-transaction self)
        (subtypep (type-of (transaction-db self)) 'database)))
  (:method ((self transaction-object)) t))

(defgeneric transaction-object (self)
  (:documentation "Return the underlying object of a transaction."))
(defgeneric transaction-store (self)
  (:documentation "Return the underlying STORE of a transaction."))
(defgeneric transaction-db (self)
  (:documentation "Return the underlying TRANSACTION-DB of a transaction. This may or may not
return the same value as DB depending on backend.")
  (:method ((self t)) *db*))
(defgeneric transaction-prior (self)
  (:documentation "Return the previous transaction of SELF if any."))

(defun known-transaction (db txn)
  "Search for a prior TXN known by this DB."
  (when txn
    (or (and (transaction-object-p txn)
             (eq db (transaction-db txn))
             txn
             (known-transaction db (transaction-prior txn))))))

;; From ELEPHANT
(defmacro with-transaction ((sym &rest initargs 
                                 &key (db '*db*)
                                      (txn '*txn*)
                                 &allow-other-keys)
                            &body body)
  "Execute a body with a transaction in place. On success,
   the transaction is committed. Otherwise, the transaction is aborted."
  (declare (ignorable db txn))
  (remf initargs :db)
  `(let ((,sym (make-transaction ,db ,@initargs)))
     ,@body))

(defmacro current-transaction (db)
  (with-gensyms (txn)
    `(let ((,txn *txn*))
       (when (and ,txn (eq (transaction-db ,txn) ,db))
         (transaction-object ,txn)))))

(defmacro ensure-transaction ((&rest initargs &key
                                              (db '*db*)
                                              (txn '*txn*)
                               &allow-other-keys)
                              &body body)
  "Execute BODY with an existing transaction or a new transaction if one does not exist.

This macro allows for the sequencing of database actions to be run atomically
inside a single transaction - use WITH-TRANSACTION if you want to nest
multiple transactions.")

