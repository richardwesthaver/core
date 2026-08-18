;;; iter.lisp --- RocksDB Iterators

;; High-level iterators for RDB/TRDB used to implement the STORE protocol.

;;; Code:
(in-package :rdb)

;;; Primitives
(defun %create-iter (db &optional (opt (rocksdb-readoptions-create)))
  (rocksdb-create-iterator db opt))

(defun %create-cf-iter (db cf &optional (opt (rocksdb-readoptions-create)))
  (rocksdb-create-iterator-cf db opt cf))

(defun %transaction-create-iter (txn &optional (opts (rocksdb-readoptions-create)))
  (rocksdb-transaction-create-iterator txn opts))

(defun %transaction-create-iter-cf (txn cf &optional (opts (rocksdb-readoptions-create)))
  (rocksdb-transaction-create-iterator-cf txn opts cf))

(defun %transactiondb-create-iter (txndb &optional (opts (rocksdb-readoptions-create)))
  (rocksdb-transactiondb-create-iterator txndb opts))

(defun %transactiondb-create-iter-cf (txndb cf &optional (opts (rocksdb-readoptions-create)))
  (rocksdb-transactiondb-create-iterator-cf txndb opts cf))

(defun %create-iterators (db opts columns)
  (with-alien ((iters (* (* rocksdb-iterator)) (make-alien (* rocksdb-iterator) (length columns)))
               (cfs (* (* rocksdb-column-family-handle)) (make-alien (* rocksdb-column-family-handle) 
                                                                     (length columns))))
    (with-errptr e (rocksdb-create-iterators db opts cfs iters e))))

(defun %reset-iter (iter) (with-errptr e (rocksdb-iter-refresh iter e)))
(defun %destroy-iter (iter) (rocksdb-iter-destroy iter))
  
(defun %writebatch-iter (self)
  (rocksdb-writebatch-iterate self nil nil (alien-callable-function 'rocksdb-delete-value)))
(defun %wbwi-iter (wbwi &key state put (deleted (sb-alien:alien-callable-function 'rocksdb-delete-value)))
  (rocksdb-writebatch-wi-iterate wbwi state put deleted))
(defun %transaction-iterator (self &key column (opts (rocksdb-readoptions-create)))
  (if column
      (%transaction-create-iter-cf self column opts)
      (%transaction-create-iter self opts)))

;;; Generators
(defmethod iter ((self rdb) &key column columns (opts (default-rocksdb-readoptions)))
  (typecase column
    (column-family (rocksdb-create-iterator-cf (db self) opts (db column)))
    (null (if columns
              (%create-iterators (db self) (options self) (mapcar 'db columns))
              (rocksdb-create-iterator (db self) opts)))
    (symbol (rocksdb-create-iterator-cf (db self) opts (db (find-column column self))))
    (simple-string (rocksdb-create-iterator-cf (db self) opts (db (find-column column self))))))

(defmethod iter ((self trdb) &key column (opts (default-rocksdb-readoptions)))
  (typecase column
    (column-family (rocksdb-transactiondb-create-iterator-cf (db self) opts (db column)))
    (null (rocksdb-transactiondb-create-iterator (db self) opts))
    (symbol (rocksdb-transactiondb-create-iterator-cf (db self) opts (db (find-column column self))))
    (simple-string (rocksdb-transactiondb-create-iterator-cf (db self) opts (db (find-column column self))))))

;;; API
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

(defun iter-delete (iter &optional cf (opts (default-rocksdb-writeoptions)) (db (db *db*)))
  (declare ((alien (* rocksdb-iterator)) iter))
  (with-errptr e
    (multiple-value-bind (key klen) (rocksdb-iter-key iter)
      (if cf
          (rocksdb-transactiondb-delete-cf db opts cf key klen e)
          (rocksdb-transactiondb-delete db opts key klen e)))))

(defun iter-move (op iter kbuf vbuf &key timestamp)
  (iter-seek op iter kbuf)
  (iter-get iter kbuf vbuf :timestamp timestamp))
