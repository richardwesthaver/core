;;; rdb/txn.lisp --- RocksDB Transactions

;;; Commentary:

;; SLICES are for write ops and PINNABLESLICES are for read ops.

;; RocksDB Slices are returned by value by the ROCKSDB-ITER-*-SLICE and
;; ROCKSDB-BATCHED-*-SLICE functions.

;; There are also 2 primary versions of PinnableSlices for various
;; operations. V1 is ROCKSDB-PINNABLESLICE and V2 is ROCKSDB-PINNABLE-HANDLE
;; which is specialized to zero-copy Get variants.

;; The former is the most common and supports a third batched structure
;; ROCKSDB-PINNABLE-MULTI-GET which is a single owner for all pinned values
;; and error messages returned by a multi-get operation.

;; The functions in this section use the BUFFER-STREAM protocol from the IO
;; system and are used to implement the RocksDB backend for the STORE
;; protocol.

;; The BUFFER slot of every BUFFER-STREAM is a SAP which is filled with a key
;; value before being sent to RocksDB, and set to the corresponding buffer
;; when retrieving a value for decoding.

;; AO <2026-08-11 Tue> we are targeting TransactionDB with a fixed-prefix.

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

;;; IO
(definline %make-slice (data size)
  (with-alien ((slice rocksdb-slice))
    (setf (slot slice 'data) data
          (slot slice 'size) size)
    slice))

(defun make-slice (stream)
  (%make-slice (buffer stream) (size stream)))

(definline %make-slice-stream (ptr len)
  "Function used to destructure a (pinable) slice into a BUFFER-STREAM. The length
and size are pre-computed."
  (declare (fixnum len) ((alien (* unsigned-char)) ptr))
  (make-instance (buffer-stream len) :buffer ptr :size len))

(defun make-pslice-stream (pslice)
  (with-pslice pslice
    (%make-slice-stream data size)))

(defun make-slice-stream (slice)
  (with-slice slice
    (%make-slice-stream data size)))

(defun slice-stream (slice stream)
  (with-slice slice
    (when (> size (buffer-stream-length stream)) (resize-buffer-stream stream size))
    (setf (size stream) size
          (buffer stream) data)))

(defmacro slice-streams (&body pairs)
  `(values ,@(loop for (k v) on pairs by #'cddr collect `(slice-stream ,k ,v))))
    
(defun pslice-stream (pslice stream)
  (with-pslice pslice
    (when (> size (buffer-stream-length stream)) (resize-buffer-stream stream size))
    (setf (size stream) size
          (buffer stream) data)))

;;; Generators
(defmethods transaction 
  (((self trdb) &key (write-opts (default-rocksdb-writeoptions))
                name
                (transaction *transaction*)
                (opts (default-rocksdb-transaction-options)))
   (unless-null-db () self
     (let ((obj (rocksdb-transaction-begin (sap self) write-opts opts transaction)))
       (when name (%set-transaction-name obj name))
       obj)))
  (((self otrdb)
    &key
    (transaction *transaction*)
    (opts (default-rocksdb-optimistictransaction-options))
    (write-opts (default-rocksdb-writeoptions)))
   (unless-null-db () self
     (rocksdb-optimistictransaction-begin (db self) write-opts opts transaction))))

;;; Transaction API
(defmethod execute ((self rdb) (fn function) &key (transaction *transaction*))
  (funcall fn)
  (when transaction
    (commit transaction)
    (rocksdb-transaction-destroy (transaction-object transaction))))

(defmethod commit ((self t) &key)
  (%commit-transaction (transaction-object self)))

(defmethod rollback ((self t) &key savepoint)
  (%rollback-transaction (transaction-object self) savepoint))

(defmethod prepare ((self t) &key)
  (%prepare-transaction (transaction-object self)))

(defmethod abort-transaction ((self t) &key savepoint)
  (%abort-transaction (transaction-object self) savepoint))

;;; TXN ops
(defun txn-get (kbuf vbuf 
                &key (transaction *transaction*)
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
                &key (transaction *transaction*)
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

(defun txn-delete (kbuf &key (transaction *transaction*) cf)
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
(defun txn-iter (&key (transaction *transaction*) cf (opts (default-rocksdb-readoptions)))
  (if cf 
      (rocksdb-transaction-create-iterator-cf transaction opts cf)
      (rocksdb-transaction-create-iterator transaction opts)))

(defun txn-iter-delete (iter &optional cf (opts (default-rocksdb-writeoptions)) (db (db *db*)))
  (declare ((alien (* rocksdb-iterator)) iter))
  (with-errptr e
    (multiple-value-bind (key klen) (rocksdb-iter-key iter)
      (if cf
          (rocksdb-transactiondb-delete-cf db opts cf key klen e)
          (rocksdb-transactiondb-delete db opts key klen e)))))

(deftype rocksdb-iterator-opcode () '(member :prev :first :next :last :for :for-prev))

(defun txn-iter-seek (op iter &optional kbuf)
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

(defun txn-iter-set (iter kbuf vbuf &key (transaction *transaction*) cf)
  "Set a key and move an iterator to its position within a
transaction. Return (values key value &optional timestamp."
  (declare ((alien (* rocksdb-iterator)) iter)
           (buffer-stream kbuf vbuf)
           ((or null (alien (* rocksdb-column-family-handle))) cf))
  (with-errptr* (e 'rdb-transaction-error :txn transaction)
    (if cf
        (rocksdb-transaction-put-cf (transaction-object transaction) cf
                                    (buffer kbuf) (size kbuf)
                                    (buffer vbuf) (size vbuf)
                                    e)
        (rocksdb-transaction-put (transaction-object transaction)
                                 (buffer kbuf) (size kbuf)
                                 (buffer vbuf) (size vbuf)
                                 e))
    (rocksdb-iter-seek iter (buffer kbuf) (size kbuf))
    (values kbuf vbuf)))

;; get pinned from iterator, optional timestamp third value.
(defun txn-iter-get (iter kbuf vbuf
                     &key timestamp)
  "Move a cursor to a key / value pair, returning the key /
value pair found.  Supports get-both and get-both-range."
  (declare ((alien (* rocksdb-iterator)) iter)
           (buffer-stream kbuf vbuf)
           ((or null buffer-stream) timestamp))
  (slice-streams 
    (rocksdb-iter-key-slice iter) kbuf
    (rocksdb-iter-value-slice iter) vbuf)
  (when timestamp (slice-stream (rocksdb-iter-timestamp-slice iter) timestamp))
  (values kbuf vbuf timestamp))

(defun txn-iter-move (op iter kbuf vbuf &key timestamp)
  (txn-iter-seek op iter kbuf)
  (txn-iter-get iter kbuf vbuf :timestamp timestamp))
