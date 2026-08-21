;;; lib/obj/db.lisp --- Database Protocol

;;

;;; TODO:

;; Template generics for ENGINE?

;;; Code:
(in-package :obj/db)

;;; Vars
(defvar *db* nil)

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

(defmacro with-db ((var &rest initargs &key (db '*db*) open close destroy &allow-other-keys) 
                   &body body)
  "Bind VAR to a database instance produced by parsing INITARGS for the extent
of BODY."
  `(let ((,var ,db))
     ,@(when open (remf initargs :open) `((open-db ,var)))
     (unwind-protect (progn ,@body)
       ,@(when close (remf initargs :close) `((close-db ,var)))
       ,@(when destroy (remf initargs :destroy) `((destroy-db ,var))))))

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
  (:documentation "Open a database and return a non-nil value if a new database was created."))

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
  (:documentation "Insert KEY:VAL into SELF, unless it already exists."))
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

#| notes

- *TRANSACTION* is bound to the current transaction being executed. A value of NIL
represents no transaction.
- should never be bound within the body of a transaction

- The macros WITH-TRANSACTION and ENSURE-TRANSACTION will always abort the
transaction in response to any non-local exit.

- WITH-TRANSACTION passes *TRANSACTION* to EXECUTE

|#

(deftype simple-transction () `(and (not null) list))

(defvar *transaction* nil
  "The current transaction or nil. 
This variable is reserved for use from within EXECUTE and should
not be rebound otherwise within the body of a transaction.")

(defclass transaction () ()
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
  (:method ((self transaction)) t))
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
