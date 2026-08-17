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

(defvar *txn* nil
  "Dynamic pointer to a ROCKSDB-TRANSACTION object.")

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

(defmethod commit ((self t) &key)
  (%commit-transaction self))

(defmethod rollback ((self t) &key savepoint)
  (%rollback-transaction self savepoint))

(defmethod prepare ((self t) &key)
  (%prepare-transaction self))

(defmethod abort-transaction ((self t) &key savepoint)
  (%abort-transaction self savepoint))

;;; TXN ops
(defun txn-get (kbuf vbuf 
                &key (transaction *txn*)
                     (opts (default-rocksdb-readoptions))
                     cf)
  "Get a key from a transaction. 
The key is encoded in a buffer-stream and on success a buffer-stream for
decoding the value is returned or NIL if nothing was found."
  (declare (buffer-stream kbuf vbuf))
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
      (declare (fixnum size))
      (when (> size (buffer-stream-length vbuf))
        (resize-buffer-stream-no-copy vbuf size))
      (setf (size vbuf) size
            (buffer vbuf) data))
    vbuf))

(defun txn-put (kbuf vbuf
                &key (transaction *txn*)
                     cf)
  "Put a key / value pair into a DB.
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

(deftype rocksdb-iterator-opcode () '(member :prev :first :next :last :for :for-prev))

(defun iter-seek (op iter &optional kbuf)
  "Set the position of an existing iterator.

Supported OPs include: :PREV :FIRST :NEXT :LAST :FOR :FOR-PREV"
  (declare ((alien (* rocksdb-iterator)) iter)
           (rocksdb-iterator-opcode op))
  (case op
    (:next (rocksdb-iter-next iter))
    (:prev (rocksdb-iter-prev iter))
    (:last (rocksdb-iter-seek-to-last iter))
    (:first (rocksdb-iter-seek-to-first iter))
    (:for (rocksdb-iter-seek iter (buffer kbuf) (size kbuf)))
    (:for-prev (rocksdb-iter-seek-for-prev iter (buffer kbuf) (size kbuf)))))

;; get pinned from iterator, optional timestamp third value.
(defun iter-get (iter kbuf vbuf
                     &key timestamp)
  "Move a cursor to a key / value pair, returning the key /
value pair found.  Supports get-both and get-both-range."
  (declare ((alien (* rocksdb-iterator)) iter)
           (buffer-stream kbuf vbuf)
           ((or null buffer-stream) timestamp))
  (set-slice-streams 
   kbuf (rocksdb-iter-key-slice iter)
   vbuf (rocksdb-iter-value-slice iter))
  (when timestamp (slice-stream (rocksdb-iter-timestamp-slice iter) timestamp))
  (values kbuf vbuf timestamp))

(defun iter-move (op iter kbuf vbuf &key timestamp)
  (txn-iter-seek op iter kbuf)
  (txn-iter-get iter kbuf vbuf :timestamp timestamp))

(defun txn-iter-delete (iter &optional cf (opts (default-rocksdb-writeoptions)) (db (db *db*)))
  (declare ((alien (* rocksdb-iterator)) iter))
  (with-errptr e
    (multiple-value-bind (key klen) (rocksdb-iter-key iter)
      (if cf
          (rocksdb-transactiondb-delete-cf db opts cf key klen e)
          (rocksdb-transactiondb-delete db opts key klen e)))))

(defun txn-iter-set (iter kbuf vbuf &key (transaction *txn*) cf)
  "Set a key and move an iterator to its position within a
transaction. Return (values key value &optional timestamp."
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
