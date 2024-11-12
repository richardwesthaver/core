;;; lib/obj/db/proto.lisp --- Database Protocol

;;

;;; Commentary:

;; This set of 

;;; Code:
(in-package :obj/db)

;;; Vars
(defvar *db* nil)
(defvar *database-backend* nil)
(defvar *default-database-collection-type* 'list)
(defvar *default-database-version* '(0 1 0))
(defvar *default-kv-size* 8)

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
  (let ((olist (gethash backend *database-backend-options*)))
    (setf (gethash backend *database-backend-options*) (pushnew option olist))))

(defun set-database-backend (backend options &rest thunks)
  (setf (gethash backend *database-backends*) thunks
        (gethash backend *database-backend-options*) options))

(declaim (inline %load-database-backend))
(defun %load-database-backend (backend)
  (dolist (th (gethash backend *database-backends*))
    (funcall th)))

(defun load-database-backend (backend)
  "Load database BACKEND and set value of *DATABASE-BACKEND*."
  (%load-database-backend backend)
  (setq *database-backend* backend))
  
;; TODO 2024-11-10: should we handle &rest/&optional too?
(defun parse-database-backend-options (initargs &optional (db-var '*db*))
  "Parse INITARGS as a plist of database options for current *DATABASE-BACKEND*."
  ;; The first element if not a keyword, is bound to the *DB* variable.
  (when (not (keywordp (car initargs)))
    (setf (symbol-value db-var) (eval (pop initargs))))
  (mapcar
   (lambda (opt)
     (let ((key (keywordicate (if (atom opt) opt (car opt)))))
       (if (member key initargs)
           (let ((match (getf initargs key)))
             (if (atom opt) (list opt match) (list (car opt) match)))
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
             (cdr opt)))
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

(defmacro with-db ((var &rest initargs) &body body)
  "Bind VAR to a DATABASE instance produced by parsing INITARGS for the extent
  of BODY."
  (let ((opts (parse-database-backend-options initargs '*db*)))
    `(let ((,var *db*))
       (prog2
           (apply 'do-database-backend-init-options ,var ',opts)
           ,@body
         (apply 'do-database-backend-close-options ,var ',opts)))))

;;; Conditions
(define-condition db-condition () ())

(deferror not-a-database (db-condition invalid-argument) ()
  (:default-initargs
   :reason "Object is not a database")
  (:auto t))

;;; Database
(defgeneric db (self)
  (:documentation "Return the Database associated with SELF."))

(defgeneric database-version (self)
  (:documentation "Return the version associated with a given database SELF."))

(defmethod database-version :around (self)
  (declare (ignorable self))
  (let ((version (call-next-method)))
    (std/macs:ifret version
                    '(0 6 0))))

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

(defclass database-schema (simple-schema id)
  ((version :accessor version :initarg :version :initform 1)
   (upgrade-schema :accessor upgrade-schema :initform nil)))

(defmethod print-object ((self database-schema) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A ~A" (id self) (version self))))

(defmethod dump-schema ((self database-schema) &optional (stream t))
  (awhen (upgrade-schema self)
    (format stream "upgrade:~%~A~%" it)))

(defun apply-schema-change-fn (instance expr old-schema)
  (cond ((functionp expr)
         (funcall expr instance))
        ((symbolp expr)
         (funcall (symbol-function expr) instance))
        ((consp expr)
         (let ((fn (compile nil (eval expr))))
           (setf (upgrade-schema old-schema) fn)
           (funcall fn instance)))))

(defclass database-collection () ()
  (:documentation "A collection of DATABASE objects."))

;; TODO 2024-05-30: maybe make into a macro?
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
    (typecase (or data-type object)
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

(defgeneric get-value (elt obj))
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
    (setf (db-opt self key) val)))
(defgeneric repair-db (self &key)
  (:documentation "Attempt to repair the database SELF."))
(defgeneric backup-db (self &key)
  (:documentation "Create a new backup for database SELF."))
(defgeneric restore-db (self from &key)
  (:documentation "Restore database SELF from object FROM."))
(defgeneric snapshot-db (self)
  (:documentation "Create a new snapshot for database SELF."))
(defgeneric write-batch (self batch &key)
  (:documentation "Write BATCH to database SELF."))
(defgeneric shutdown-db (self &key)
  (:documentation "Shutdown database SELF."))
(defgeneric ingest-into-db (self file &key)
  (:documentation "Ingest an external file into the database"))

;; Merge Ops
(defgeneric merge-key (self key val &key)
  (:documentation "Perform a merge operation on SELF using KEY and VAL."))
(defgeneric merge-kv (self kv &key)
  (:documentation "Perform a merge operation on SELF using object KV."))

;; Columns (column families)
(defgeneric open-columns (self &rest names)
  (:documentation "Open the columns indicated by NAMES or all columns belonging
to SELF."))
(defgeneric close-column (self &optional error)
  (:documentation "Close the column SELF. When ERROR is non-nil signal an error if the
column is already closed."))
(defgeneric close-columns (self)
  (:documentation "Close the columns belonging to SELF."))
(defgeneric create-columns (self)
  (:documentation "Create the columns belonging to SELF."))
(defgeneric find-column (cf self &key)
  (:documentation "Find the column COL in SELF."))
(defgeneric flush-column (self col &key)
  (:documentation "Flush the column COL in SELF."))
(defgeneric add-column (col self)
  (:documentation "Add a column to SELF."))
(defgeneric columns (self)
  (:documentation "Return the columns of SELF."))
(defgeneric column (self col))
(defgeneric (setf column) (new self col))

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
(defgeneric make-transaction (self txn &key)
  (:documentation "Make a new transaction object."))
(defgeneric prepare-transaction (self txn &key)
  (:documentation "Prepare a transaction."))
(defgeneric rollback-transaction (self txn &key)
  (:documentation "Rollback a transaction."))
(defgeneric delete-transaction (self txn &key)
  (:documentation "Delete transaction SELF."))
(defgeneric commit-transaction (self txn &key)
  (:documentation "Commit transaction object SELF."))

(defgeneric execute-transaction (self txfn &rest args &key &allow-other-keys))
(defgeneric start-transaction (self transaction &key &allow-other-keys))
(defgeneric stop-transaction (self transaction &key &allow-other-keys))
(defgeneric abort-transaction (self transaction &key &allow-other-keys))

(defvar *txn* nil)

(defclass transaction-object () ())

(defgeneric transaction-object-p (self)
  (:method ((self t))
    (and (not (null self))
         (consp self)
         (subtypep (type-of (transaction-db self)) 'database)))
  (:method ((self transaction-object)) t))

(defgeneric transaction-object (self))
(defgeneric transaction-store (self))
(defgeneric transaction-db (self))
(defgeneric transaction-prior (self))

(defun known-transaction (db txn)
  "Search for a prior TXN known by this DB."
  (when txn
    (or (and (transaction-object-p txn)
             (eq db (transaction-db txn))
             txn
             (known-transaction db (transaction-prior txn))))))

(defmacro with-transaction ((&rest args &key 
                                        (db '*db*)
                                        (parent '*txn*)
                             &allow-other-keys)
                            &body body)
  "Execute a body with a transaction in place.  On success,
   the transaction is committed.  Otherwise, the transaction is
   aborted.  If the body deadlocks, the body is re-executed in
   a new transaction, retrying a fixed number of iterations.
   If nested, the backend must support nested transactions."
  (once-only (db)
    (with-gensyms (txn-fn)
      `(let ((,txn-fn (lambda () ,@body)))
         (funcall #'execute-transaction ,db
                  ,txn-fn
                  :parent (awhen (known-transaction ,db ,parent)
                            (transaction-object it))
                  ,@(progn
                      (dolist (k '(:db :parent))
                        (remf args k))
                      args))))))
  
(defmacro ensure-transaction ((&rest args &key
                                     (db '*db*)
                                     (parent '*txn*)
                                     &allow-other-keys)
                              &body body)
  "Execute the body with the existing transaction, or a new transaction if
   none is currently running.  This allows sequences of database actions to 
   be run atomically whether there is or is not an existing transaction 
   (rather than relying on auto-commit).  with-transaction nests transactions
   where as ensure-transaction can be part of an enclosing, flat transaction"
  (once-only (db)
    (with-gensyms (txn-fn)
    `(let ((,txn-fn (lambda () ,@body)))
       (if (known-transaction ,db ,parent)
           (funcall ,txn-fn)
           (funcall #'execute-transaction ,db
                    ,txn-fn
                    :parent nil
                    ,@(progn
                        (dolist (k '(:db :parent))
                          (remf args k))
                        args)))))))

(defmacro with-batch-transaction ((batch size list &rest txn-options) &body body)
  "Perform a set of DB operations over a list of elements in batches of size 'size'.
   Pass specific transaction options after the list reference."
  `(loop for ,batch in (subsets ,size ,list) do
        (with-transaction ,txn-options
          ,@body)))

(defmacro current-transaction (db)
  (with-gensyms (txn)
    `(let ((,txn *txn*))
       (when (and ,txn (eq (transaction-db ,txn) ,db))
         (transaction-object ,txn)))))
