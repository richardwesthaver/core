;;; lib/obj/db/proto.lisp --- Database Protocol

;;

;;; TODO:

;; Template generics for backends

;;; Code:
(in-package :obj/db)

;;; Vars
(defvar *db* nil)
(defvar *database-backend* nil)
(defparameter *save-database-backend-on-load* t)
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
    (setf (options db) val))
  (:method (db (key (eql :opt)) (val cons))
    (setf (opt db (car val)) (cdr val)))
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
(defgeneric (setf db) (new self)
  (:documentation "Set the Database associated with SELF."))

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
Every database has at least one slot named DB which points to the raw database
handle."))

(defgeneric make-db (engine &rest initargs &key &allow-other-keys)
  (:documentation "Dispatch initializer for databases. An ENGINE must be supplied, which is
usually a key such as :ROCKSDB or :SQLITE."))

(defgeneric close-db (db &key &allow-other-keys)
  (:documentation "Close a database."))

(defgeneric open-db (self)
  (:documentation "Open a database."))

(defgeneric destroy-db (self)
  (:documentation "Destroy all traces of a database, deleting any on-disk data and shutting down
in-memory objects."))

(defgeneric db-open-p (self)
  (:documentation "Return T when database SELF is open.")
  (:method ((self t)) (invalid-database self))
  (:method ((self database)) (when (db self) t)))

(defgeneric db-closed-p (self)
  (:documentation "Return T when database SELF is closed.")
  (:method ((self t)) (invalid-database self))
  (:method ((self database)) (unless (db self) t)))

;;; Common
(defgeneric put-key (self key val &key)
  (:documentation "Insert a KEY and VAL."))
(defgeneric get-key (self key &key)
  (:documentation "Get value of KEY."))
(defgeneric multi-get (self keys &key)
  (:documentation "Retrieve multiple KEYS from SELF."))
(defgeneric insert-key (self key val &key)
  (:documentation "Insert KEY:VAL into SELF."))
(defgeneric delete-key (self key &key)
  (:documentation "Delete value associated with KEY from SELF."))
(defgeneric remove-kv (key value self))
(defgeneric snapshot (self &key)
  (:documentation "Create a new snapshot for database SELF."))
(defgeneric checkpoint (self &key)
  (:documentation "Create a new checkpoint for database SELF."))
(defgeneric write-batch (self batch &key)
  (:documentation "Write BATCH to database SELF."))
(defgeneric shutdown-db (self &key wait &allow-other-keys)
  (:documentation "Shutdown database SELF."))
(defgeneric ingest-db (self file &key)
  (:documentation "Ingest external files into a database."))
(defgeneric backup (self &key))

;;; Config
(defconfig db-config ()
  ((engine :initform nil :initarg :engine :allocation :class)
   (options :initarg :options :accessor options)))

(defconfig simple-db-config (ast id db-config)
  ((path :initarg :path :initform nil :accessor path)
   (schema :initarg :schema :accessor schema)))

;; Merge Ops
(defgeneric merge-key (self key val &key)
  (:documentation "Perform a merge operation on SELF using KEY and VAL."))

;; TODO 2026-08-09: 
(defmacro with-merge-op ())

;; Columns
(defgeneric make-column (self &key &allow-other-keys)
  (:documentation "Create a column from SELF."))
(defgeneric find-column (col self &key &allow-other-keys)
  (:documentation "Find the column COL in SELF."))
(defgeneric (setf find-column) (new col self &key)
  (:documentation "Find the column COL in SELF."))

;;; Transactions

;; In our system, transactions must at least implement a DB method which
;; returns an instance of DATABASE.

#| notes

- *TRANSACTION* is bound to the current transaction being executed. A value of NIL
   represents no transaction. The current *DATABASE-BACKEND* may modify this
   variable within the EXECUTE method.
   - should never be bound within the body of a transaction

- The macros WITH-TRANSACTION and ENSURE-TRANSACTION will always abort the
  transaction in response to any non-local exit.

- WITH-TRANSACTION passes *TRANSACTION* to EXECUTE

|#
(deftype simple-transaction () `(and (not null) list))

(defun simple-transaction (store txn &optional prior)
  (list store txn prior))

(defvar *transaction* nil
  "The current transaction or nil. 
This variable is reserved for use from within EXECUTE and should
not be rebound otherwise within the body of a transaction.")

(defclass transaction-object () ()
  (:documentation "Base class for transaction objects."))

(defkernel transaction-kernel (transaction kernel-object) ()
  (:documentation "Kernel object for transactions.
The funcallable-instance may be used to respect a simple commit-based protocol
which mirrors the backend."))

(defgeneric transaction (self &key &allow-other-keys)
  (:documentation "Make a new transaction object."))

(defgeneric prepare (self &key)
  (:documentation "Prepare a transaction."))

(defgeneric rollback (self &key)
  (:documentation "Rollback a transaction."))

(defgeneric commit (self &key)
  (:documentation "Commit a transaction."))

(defgeneric execute (self kernel &rest args &key &allow-other-keys)
  (:documentation
   "Interface to the backend transaction kernel (a function). The body of the
kernel function should be executed in an environment that protects against
non-local exits, provides ACIDic properties and binds any relevant parameters."))

(defgeneric abort-transaction (self &key &allow-other-keys))

(defgeneric transactionp (self)
  (:documentation "Return Non-nil if SELF is a transaction object.")
  (:method ((self t))
    (or (typep self 'simple-transaction)
        (subtypep (type-of (transaction-db self)) 'database)))
  (:method ((self transaction-object)) t))

(defgeneric transaction-object (self)
  (:documentation "Return the underlying object of a transaction.")
  (:method ((self list)) (second self))
  (:method ((self alien-value)) self)
  (:method ((self system-area-pointer)) self))
(defgeneric transaction-store (self)
  (:documentation "Return the underlying STORE of a transaction.")
  (:method ((self list)) (first self))
  (:method ((self t)) nil))
(defgeneric transaction-db (self)
  (:documentation "Return the underlying TRANSACTION-DB of a transaction. This may or may not
return the same value as DB depending on backend.")
  (:method ((self t)) (db (transaction-store self))))
(defgeneric transaction-prior (self)
  (:documentation "Return the previous transaction of SELF if any.")
  (:method ((self list)) (third self))
  (:method ((self t)) nil))

(defun known-transaction (db txn)
  "Search for a prior TXN known by this DB."
  (and txn
       (transactionp txn)
       (or (eq db (transaction-db txn))
           (eq (transaction-db db) (transaction-db txn))
           (eq db (transaction-store txn))
           (eq (transaction-store db) (transaction-store txn)))
       (known-transaction db (transaction-prior txn))))

(define-condition transaction-retry-count-exceeded (error)
  ((count :initarg :count :accessor retry-count :initform 0)))

(define-condition transaction-error (db-error)
  ((transaction :initform *transaction* :initarg :transaction :reader error-transaction)))

(defvar *default-transaction-wait* 0.1)
(defvar *default-transaction-retry* 0)

;; From ELEPHANT
(defmacro with-transaction ((&rest initargs 
                             &key (db '*db*)
                                  (store '*store*)
                                  (transaction '*transaction*)
                                  ;; retries wait
                             &allow-other-keys)
                            &body body)
  "Execute a body with a transaction in place. On success, the transaction is
committed. Otherwise, the transaction is aborted."
  (with-gensyms (%txn-fn)
    (remf initargs :db)
    (remf initargs :store)
    (remf initargs :transaction)
    `(let ((*db* ,db)
           (*store* ,store)
           (*transaction* ,transaction))
       (let ((,%txn-fn (lambda () ,@body)))
         (funcall #'execute *db* ,%txn-fn 
                  :transaction (aif (known-transaction *db* *transaction*) (transaction-object it) it)
                  ,@initargs)))))

(defmacro current-transaction (db)
  "Return the current transaction associated with database DB."
  (with-gensyms (txn)
    `(let ((,txn *transaction*))
       (when (and ,txn (eq (transaction-db ,txn) ,db))
         (transaction-object ,txn)))))

(defmacro ensure-transaction ((&rest initargs 
                               &key
                               (db '*db*)
                               (store '*store*)
                               (transaction '*transaction*)
                               wait
                               &allow-other-keys)
                              &body body)
  "Execute BODY with an existing transaction or a new transaction if one does not
exist. This macro allows for the sequencing of database actions to be run
atomically regardless of whether there is an existing transaction or not."
  (with-gensyms (%db %txn-fn)
    (remf initargs :db)
    (remf initargs :store)
    (remf initargs :transaction)
    (remf initargs :wait)
    `(let ((,%db (or ,db ,store))
           (,%txn-fn (lambda () ,@body)))
       (if (known-transaction ,%db ,transaction)
           (funcall ,%txn-fn)
           (funcall #'execute ,%db
                    ,%txn-fn
                    :transaction nil
                    ,@(when wait `(:wait ,wait)))))))

(defmacro with-batch-transaction ((batch size list &rest txn-options) &body body)
  "Perform a set of DB operations over a sequence of elements LIST in batches of
SIZE. Transaction keywords accepted by WITH-TRANSACTION are accepted
immediately following LIST."
  `(loop for ,batch in (group ,list ,size)
         do (with-transaction ,txn-options
              ,@body)))

(defgeneric db-cursor (db &key &allow-other-keys)
  (:documentation "Return a database cursor, default to calling [[STD/SEQ:ITER][iter]]."))

;;; Catalog
;; TODO 2026-08-09: 
;; (defclass catalog () ())
