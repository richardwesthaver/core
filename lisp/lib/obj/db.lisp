;;; lib/obj/db/proto.lisp --- Database Protocol

;;

;;; Commentary:

;; This set of 

;;; Code:
(in-package :obj/db)

(defvar *db* nil)

;;; Vars
(declaim (sb-kernel:type-specifier *default-database-type* *default-database-collection-type*))
(defparameter *default-database-type* 'vector)
(defparameter *default-database-collection-type* 'list)
(defparameter *default-database-version* '(0 1 0))
;;; Conditions
(define-condition db-condition () ())

(deferror not-a-database (db-condition invalid-argument) ()
  (:default-initargs
   :reason "Object is not a database"))

(defun not-a-database (item) (error 'not-a-database :item item))
  
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

(defgeneric get-val (object element &optional data-type)
  (:documentation "Returns the value in a object based on the supplied element name and possible
type hints.")
  (:method (object element &optional data-type)
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

(defgeneric (setf get-val) (new-value object element &optional data-type)
  (:documentation "Set the value in a object based on the supplied element name and possible type
hints.")
  (:method (new-value object element &optional data-type)
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

;;; Transactions
(defgeneric execute-transaction (self txfn &rest args &key &allow-other-keys))
;; Explicit control
(defgeneric start-transaction (self transaction &key &allow-other-keys))
(defgeneric stop-transaction (self transaction &key &allow-other-keys))
(defgeneric abort-transaction (self transaction &key &allow-other-keys))

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
(defgeneric make-transaction (self &key)
  (:documentation "Make a new transaction object from SELF."))
(defgeneric prepare-transaction (self &key)
  (:documentation "Prepare transaction SELF."))
(defgeneric rollback-transaction (self &key)
  (:documentation "Rollback transaction SELF."))
(defgeneric delete-transaction (self)
  (:documentation "Delete transaction SELF."))
(defgeneric commit-transaction (self &key)
  (:documentation "Commit transaction object SELF."))
(defgeneric flush-db (self &key)
  (:documentation "Flush the database SELF."))
(defgeneric sync-db (self other &key) ;;nyi
  (:documentation "Perform a synchronization on SELF using OTHER."))

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

(defvar *default-kv-size* 8)

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
(defvar *txn* nil)

(defgeneric transaction-object (self))

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
                  :parent (awhen (known-transaction-p ,db ,parent)
                            (transaction-object it))
                  ,@(progn
                      (dolist (k '(:db :parent))
                        (remf args k))
                      args))))))

(defgeneric known-transaction-p (db txn))

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
       (if (known-transaction-p ,db ,parent)
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
