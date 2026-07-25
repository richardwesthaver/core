;;; lib/obj/db/proto.lisp --- Database Protocol

;;

;;; TODO:

;; Template generics for backends

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
(defvar *database-backend-table* (make-hash-table)
  "Hash Table where keys are a database backend designator and values
are a list of functions which are responsible for doing all initialization
such as loading shared libraries and setting variables.")

(defvar *database-backend-options* (make-hash-table)
  "Hash Table where keys are a database backend designator and values are a
lambda-list which will be interpreted by PARSE-DATABASE-BACKEND-OPTIONS within
the body of WITH-DB forms.")

(defvar *database-backend-close-options* '(close destroy))

(defun add-database-loader (backend thunk)
  (let ((flist (gethash backend *database-backend-table*)))
    (setf (gethash backend *database-backend-table*) (pushnew thunk flist :test 'equalp))))

(defun add-database-backend-option (backend option)
  "Add a new database backend option."
  (let ((olist (gethash backend *database-backend-options*)))
    (setf (gethash backend *database-backend-options*) (pushnew option olist))))

(defun set-database-backend (backend options &rest thunks)
  "Set the loaders (a sequence of thunks) and options for the designated database
backend keyword BACKEND."
  (setf (gethash backend *database-backend-table*) thunks
        (gethash backend *database-backend-options*) options))

(declaim (inline %load-database-backend))
(defun %load-database-backend (backend)
  (when-let ((be (gethash backend *database-backend-table*)))
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

;;; Conditions
(defcondition db-condition () ()
  (:documentation "Superclass for DB conditions.")
  (:error-class db-error (error) ())
  (:warning-class db-warning (warning) ()))

(deferror invalid-database (db-error invalid-argument) ()
  (:documentation "Error signaled when an invalid DB is detected.")
  (:default-initargs
   :reason "Object is not a database"))

(defun invalid-database (item)
  (error 'invalid-database :item item))

;; TODO 2025-08-12: call-with
;; (defun call-with-db (db fn &rest args))

(defmacro with-db ((var &rest initargs &key (db '*db*) &allow-other-keys) 
                   &body body)
  "Bind VAR to a DATABASE instance produced by parsing INITARGS for the extent
  of BODY which may contain any of the *DATABASE-BACKEND-OPTIONS* available
  for the current *DATABASE-BACKEND*."
  (with-gensyms (opts)
    `(let ((,opts ',(parse-database-backend-options initargs))
           (,var ,db))
       ;; ,@(when open (remf initargs :open) `((open-db ,var)))
       (apply 'do-database-backend-init-options ,var ,opts)
       (unwind-protect (progn ,@body)
         ;; ,@(when close (remf initargs :close) `((close-db ,var)))
         ;; ,@(when destroy (remf initargs :destroy) `((destroy-db ,var)))
         (apply 'do-database-backend-close-options ,var ,opts)))))

;;; Database
(defgeneric db (self)
  (:documentation "Return the Database associated with SELF."))

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
  (:documentation "Base class for Database objects.
Every database has at least one slot named DB."))

(defclass database-collection () ()
  (:documentation "A collection of DATABASE objects."))

(defgeneric make-db (engine &rest initargs &key &allow-other-keys)
  (:documentation "Dispatch initializer for databases. An ENGINE must be supplied, which is
usually a key such as :ROCKSDB or :SQLITE."))

(defgeneric connect-db (db &key &allow-other-keys)
  (:documentation "Connect the database DB."))

(defgeneric close-db (db &key &allow-other-keys)
  (:documentation "Close a database."))

(defgeneric open-db (self)
  (:documentation "Open a database."))

(defgeneric destroy-db (self)
  (:documentation "Destroy all traces of a database, deleting any on-disk data and shutting down
in-memory objects."))

(defgeneric find-db (name dbs &key &allow-other-keys)
  (:documentation "Return the db by NAME, from a collection of databases DBS."))

(defgeneric insert-db (name dbs &key &allow-other-keys)
  (:documentation "Inserts a database by NAME into the database-collection DBS."))

(defgeneric db-open-p (self)
  (:documentation "Return T when database SELF is open.")
  (:method ((self t)) (invalid-database self))
  (:method ((self database)) (when (db self) t)))

(defgeneric db-closed-p (self)
  (:documentation "Return T when database SELF is closed.")
  (:method ((self t)) (invalid-database self))
  (:method ((self database)) (unless (db self) t)))

;;; Common
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
(defgeneric ingest-db (self file &key)
  (:documentation "Ingest external files into a database."))

;;; Config
(defconfig db-config ()
  ((backend :initform nil :initarg :backend)
   (options :initarg :options :accessor db-opts)))

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

;;; Transactions

;; In our system, transactions must at least implement a TRANSACTION-DB method
;; which returns an instance of DATABASE

;; Simple transactions are non-nil lists which are handled according to the
;; current database backend.

#| notes

- *TXN* is bound to the current transaction being executed. A value of NIL
   represents no transaction. The current *DATABASE-BACKEND* may modify this
   variable within the EXECUTE-TRANSACTION method.
   - should never be bound within the body of a transaction

- The macros WITH-TRANSACTION and ENSURE-TRANSACTION will always abort the
  transaction in response to any non-local exit.

- WITH-TRANSACTION passes *TXN* to EXECUTE-TRANSACTION

|#
(deftype simple-transaction () `(and (not null) list))

(defvar *default-txn* '(nil nil nil))
(defvar *txn* nil
  "The current transaction.")

(defclass transaction-object () ()
  (:documentation "Base class for transaction objects."))

(defclass transaction-kernel (kernel-object transaction-object) ()
  (:documentation "Kernel object for transactions.")
  (:metaclass kernel-class))

(defgeneric (setf transaction-opts) (new txn))

(defgeneric make-transaction (self &key &allow-other-keys)
  (:documentation "Make a new transaction object.")
  (:method ((self null) &key) *default-txn*))

(defgeneric prepare-transaction (self &key)
  (:documentation "Prepare a transaction."))

(defgeneric rollback-transaction (self &key)
  (:documentation "Rollback a transaction."))

(defgeneric commit-transaction (self &key)
  (:documentation "Commit a transaction."))

(defgeneric execute-transaction (self kernel &rest args &key &allow-other-keys)
  (:documentation
   "Interface to the backend transaction kernel (a function). The body of the
kernel function should be executed in an environment that protects against
non-local exits, provides ACIDic properties and binds any relevant parameters."))

(defgeneric abort-transaction (self &key &allow-other-keys))

(defgeneric transaction-object-p (self)
  (:documentation "Return Non-nil if SELF is a transaction object.")
  (:method ((self t))
    (or (typep 'simple-transaction self)
        (subtypep (type-of (transaction-db self)) 'database)))
  (:method ((self transaction-object)) t))

(defgeneric transaction-object (self)
  (:documentation "Return the underlying object of a transaction.")
  (:method ((self list)) (second self)))
(defgeneric transaction-store (self)
  (:documentation "Return the underlying STORE of a transaction.")
  (:method ((self list)) (first self))
  (:method ((self t)) nil))
(defgeneric transaction-db (self)
  (:documentation "Return the underlying TRANSACTION-DB of a transaction. This may or may not
return the same value as DB depending on backend.")
  (:method ((self t)) *db*))
(defgeneric transaction-prior (self)
  (:documentation "Return the previous transaction of SELF if any.")
  (:method ((self list)) (third self))
  (:method ((self t)) nil))

(defun known-transaction (db txn)
  "Search for a prior TXN known by this DB."
  (when txn
    (or (and txn
             (transaction-object-p txn)
             (or (eq db (transaction-db txn))
                 (eq (transaction-db db) (transaction-db txn))
                 (eq db (transaction-store txn))
                 (eq (transaction-store db) (transaction-store txn)))
             (known-transaction db (transaction-prior txn))))))

(define-condition transaction-retry-count-exceeded (error)
  ((count :initarg :count :accessor retry-count :initform 0)))

(defvar *default-txn-wait* 0.1)
(defvar *default-txn-retry* 0)

;; From ELEPHANT
(defmacro with-transaction ((&rest initargs 
                             &key (db '*db*)
                                  (store '*store*)
                                  (txn '*txn*)
                                  ;; retries wait
                             &allow-other-keys)
                            &body body)
  "Execute a body with a transaction in place. On success, the transaction is
committed. Otherwise, the transaction is aborted."
  (with-gensyms (%txn-fn)
    (remf initargs :db)
    (remf initargs :store)
    (remf initargs :txn)
    `(let ((*db* ,db)
           (*store* ,store)
           (*txn* ,txn))
       (let ((,%txn-fn (lambda () ,@body)))
         (funcall #'execute-transaction *db* ,%txn-fn 
                  :txn (aif (known-transaction *db* *txn*) (transaction-object it) it)
                  ,@initargs)))))

(defmacro current-transaction (db)
  "Return the current transaction associated with database DB."
  (with-gensyms (txn)
    `(let ((,txn *txn*))
       (when (and ,txn (eq (transaction-db ,txn) ,db))
         (transaction-object ,txn)))))

(defmacro ensure-transaction ((&rest initargs 
                               &key
                               (db '*db*)
                               (store '*store*)
                               (txn '*txn*)
                               retries wait
                               &allow-other-keys)
                              &body body)
  "Execute BODY with an existing transaction or a new transaction if one does not
exist. This macro allows for the sequencing of database actions to be run
atomically regardless of whether there is an existing transaction or not."
  (with-gensyms (%db %txn-fn)
    (remf initargs :db)
    (remf initargs :store)
    (remf initargs :txn)
    (remf initargs :retries)
    (remf initargs :wait)
    `(let ((,%db (or ,db ,store))
           (,%txn-fn (lambda () ,@body)))
       (if (known-transaction ,%db ,txn)
           (funcall ,%txn-fn)
           (funcall #'execute-transaction ,%db
                    ,%txn-fn
                    :txn nil
                    ,@(when retries `(:retries ,retries))
                    ,@(when wait `(:wait ,wait)))))))

(defmacro with-batch-transaction ((batch size list &rest txn-options) &body body)
  "Perform a set of DB operations over a sequence of elements LIST in batches of
SIZE. Transaction keywords accepted by WITH-TRANSACTION are accepted
immediately following LIST."
  `(loop for ,batch in (group ,list ,size)
         do (with-transaction ,txn-options
              ,@body)))
