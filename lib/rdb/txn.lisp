;;; rdb/txn.lisp --- TXN API

;;; Commentary:

;; Indices are an important part of DB design and we implement them in a
;; similar way to MyRocks which leverages a prefix_extractor and a flat
;; single-table structure.

;; Our implementation is simpler in that every index-like table (with multiple
;; non-unique prefixes) is supported by an additional lookup-table storing a
;; mapping of KEY:IDX. To seek to a different prefix in an index-iterator we
;; first move the lookup-iterator which is opened with ~(:total-order-seek t)~
;; and use the IDX to query the index table.

;;; Code:
(in-package :rdb)

(defvar *txn* nil "Dynamic pointer to a ROCKSDB-TRANSACTION object.")

(define-condition rdb-transaction-error (rdb-alien-error transaction-error)
  ((txn :initarg :txn :initform *txn* :reader error-txn))
  (:documentation "Error signaled in the context of a Transaction."))

;; (defun store-txn (store txn &optional prior)
;;   (list store txn prior))

;;; Primitives
(defun %transaction-wbwi (self) (rocksdb-transaction-get-writebach-wi self))

(defun %commit-transaction (txn)
  (with-errptr* (e 'rdb-alien-error)
    (rocksdb-transaction-commit txn e)))

(defun %set-savepoint (txn)
  (rocksdb-transaction-set-savepoint txn))

(defun %rollback-transaction (txn &optional savepoint)
  "Rollback a raw transaction TXN when SAVEPOINT is non-nil only rollback to last
savepoint created with ROCKSDB-TRANSACTION-SET-SAVEPOINT."
  (with-errptr* (e 'rdb-alien-error)
    (if savepoint
        (rocksdb-transaction-rollback-to-savepoint txn e)
        (rocksdb-transaction-rollback txn e))))

(defun %prepare-transaction (txn)
  (with-errptr* (e 'rdb-transaction-error :txn txn)
    (rocksdb-transaction-prepare txn e)))

(defun %abort-transaction (self &optional savepoint)
  (%rollback-transaction self savepoint)
  (rocksdb-transaction-destroy self))

(defun %get-prepared-transactions (txn-db)
  "Return an array of prepared ROCKSDB-TRANSACTION pointers from this
transaction-db."
  (with-errptr* (e 'rdb-alien-error :db txn-db)
    (rocksdb-transactiondb-get-prepared-transactions txn-db)))

;;; Generators
(defmethods transaction 
  (((self trdb) &key (write-opts (default-rocksdb-writeoptions))
                name
                (transaction *txn*)
                (opts (default-rocksdb-transaction-options)))
   (unless-null-db () self
     (let ((obj (rocksdb-transaction-begin (sap self) write-opts opts transaction)))
       (when name (%set-transaction-name obj name))
       obj)))
  (((self otrdb)
    &key
    name
    (transaction *txn*)
    (opts (default-rocksdb-optimistictransaction-options))
    (write-opts (default-rocksdb-writeoptions)))
   (unless-null-db () self
     (let ((obj (rocksdb-optimistictransaction-begin (db self) write-opts opts transaction)))
       (when name (%set-transaction-name obj name))
       obj))))

;;; Default Transaction API
(defmethod execute ((self rdb) (fn function) &key (transaction *txn*))
  (funcall fn)
  (when transaction
    (commit transaction)
    (rocksdb-transaction-destroy transaction)))

(defmethod commit ((self alien-value) &key)
  (%commit-transaction self))

(defmethod rollback ((self alien-value) &key savepoint)
  (%rollback-transaction self savepoint))

(defmethod prepare ((self alien-value) &key)
  (%prepare-transaction self))

(defmethod abort-transaction ((self alien-value) &key savepoint)
  (%abort-transaction self savepoint))

;;; TXN ops
(defun txn-get (kbuf
                &key (transaction *txn*)
                     (opts (default-rocksdb-readoptions))
                     cf)
  "Get a key from a transaction. 
The key is encoded in a buffer-stream and on success a buffer-stream for
decoding the value is returned or NIL if nothing was found."
  (declare (buffer-stream kbuf))
  (with-errptr* (e 'rdb-transaction-error :txn transaction)
    (with-pslice
      (if cf
          (rocksdb-transaction-get-pinned-cf
           transaction opts cf
           (buffer kbuf)
           (size kbuf)
           e)
          (rocksdb-transaction-get-pinned
           transaction opts
           (buffer kbuf)
           (size kbuf)
           e))
      (make-instance (buffer-stream size) :size size :buffer data))))

(defun txn-put (kbuf vbuf
                &key (transaction *txn*)
                     cf)
  "Put a kv pair into a DB.
The pair are encoded in buffer-streams."
  (declare ((alien (* rocksdb-transaction)) transaction)
           (buffer-stream kbuf vbuf))
  (with-errptr* (e 'rdb-transaction-error :txn transaction)
    (if cf
        (rocksdb-transaction-put-cf
         transaction
         cf
         (buffer kbuf)
         (size kbuf)
         (buffer vbuf)
         (size vbuf)
         e)
        (rocksdb-transaction-put 
         transaction 
         (buffer kbuf)
         (size kbuf)
         (buffer vbuf)
         (size vbuf)
         e))))

(defun txn-insert (db kbuf vbuf &key (transaction *txn*) cf)
  (unless-key-exists (kbuf db :cf cf)
    (txn-put kbuf vbuf :transaction transaction :cf cf)))

(defun txn-delete (kbuf &key (transaction *txn*) cf)
  "Delete a key / value pair from a DB.
The key is encoded in a buffer-stream. T on success, NIL if the key wasn't
found."
  (declare ((alien (* rocksdb-transaction)) transaction)
           (buffer-stream kbuf))
  (with-errptr* (e 'rdb-transaction-error :txn transaction)
    (if cf
        (rocksdb-transaction-delete-cf transaction cf (buffer kbuf) (size kbuf) e)
        (rocksdb-transaction-delete transaction (buffer kbuf) (size kbuf) e))))

;;; Transaction Iterator
(defun txn-iter (&key (transaction *txn*) cf (opts (default-rocksdb-readoptions)))
  (if cf 
      (rocksdb-transaction-create-iterator-cf transaction opts cf)
      (rocksdb-transaction-create-iterator transaction opts)))

(defun txn-iter-set (iter kbuf vbuf &key (transaction *txn*) cf)
  "Set a key and move an iterator to its position within a
transaction. Return (values key value)."
  (declare ((alien (* rocksdb-iterator)) iter)
           (buffer-stream kbuf vbuf)
           ((or null (alien (* rocksdb-column-family-handle))) cf))
  (with-errptr* (e 'rdb-transaction-error :txn transaction)
    (if cf
        (rocksdb-transaction-put-cf transaction cf
                                    (buffer kbuf) (size kbuf)
                                    (buffer vbuf) (size vbuf)
                                    e)
        (rocksdb-transaction-put transaction
                                 (buffer kbuf) (size kbuf)
                                 (buffer vbuf) (size vbuf)
                                 e))
    (rocksdb-iter-seek iter (buffer kbuf) (size kbuf))
    (values kbuf vbuf)))
