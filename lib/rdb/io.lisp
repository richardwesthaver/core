;;; rdb/io.lisp --- IO Primitives

;; RDB support for STATIC/BUFFER-STREAM APIs.

;;; Commentary:

;; The functions in this section use the BUFFER-STREAM protocol from the IO
;; system and are used to implement the RocksDB backend for the STORE
;; protocol.

;; The BUFFER slot of every BUFFER-STREAM is a SAP which is filled with a key
;; value before being sent to RocksDB, and set to the corresponding value of a
;; PinnableSlice C struct when retrieving a value for decoding.

;; AO <2026-08-11 Tue> we are targeting TransactionDB only.

;;; Code:
(in-package :rdb)

(defmacro txn-default (dvar)
  `(progn
     (assert (null ,dvar))
     nil))

(defmacro with-slice ((data size) slice &body body)
  "Eval BODY with the pinnable-slice pointer SLICE destructured into DATA and SIZE values."
  `(multiple-value-bind (,data ,size) (rocksdb::rocksdb-pinnableslice-value ,slice)
     ,@body
     (rocksdb::rocksdb-pinnableslice-destroy ,slice)))

(defun txn-get-key-buffered (kbuf vbuf 
                             &key (transaction (txn-default *transaction*)) 
                                  (opts (default-rocksdb-readoptions))
                                  cf)
  "Get a key from a transaction. 
The key is encoded in a buffer-stream and on success a buffer-stream for
decoding the value is returned or NIL if nothing was found."
  (declare (buffer-stream kbuf vbuf))
  (with-errptr* (e 'rdb-transaction-error :txn transaction)
    (with-slice (val result-size)
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
      (declare (fixnum result-size))
      (when (> result-size (buffer-stream-length vbuf))
        (resize-buffer-stream-no-copy vbuf result-size))
      (setf (size vbuf) result-size
            (buffer vbuf) val))
    vbuf))

(defun txn-put-buffered (kbuf vbuf
                         &key (transaction (txn-default *transaction*))
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

(defun txn-delete-buffered  (kbuf &key (transaction (txn-default *transaction*)) cf)
  "Delete a key / value pair from a DB.
The key is encoded in a buffer-stream. T on success, NIL if the key wasn't
found."
  (declare ((alien (* rocksdb-transaction)) transaction)
           (buffer-stream kbuf))
  (with-errptr* (e 'rdb-transaction-error :txn transaction)
    (if cf
        (rocksdb-transaction-delete-cf transaction cf (buffer kbuf) (size kbuf) e)
        (rocksdb-transaction-delete transaction (buffer kbuf) (size kbuf) e))))

;;; Cursors
(defun txn-cursor (&key (transaction (txn-default *transaction*)) cf (opts (default-rocksdb-readoptions)))
  (if cf 
      (rocksdb-transactiondb-create-iterator-cf transaction opts cf)
      (rocksdb-transactiondb-create-iterator transaction opts)))

(defun txn-cursor-delete (cursor &optional cf (opts (default-rocksdb-writeoptions)) (db (db *db*)))
  (declare ((alien (* rocksdb-iterator)) cursor))
  (with-errptr e
    (multiple-value-bind (key klen) (rocksdb-iter-key cursor)
      (if cf
          (rocksdb-transactiondb-delete-cf db opts cf key klen e)
          (rocksdb-transactiondb-delete db opts key klen e)))))

(defun txn-cursor-move-buffered (cursor kbuf vbuf &key current first last next prev)
  "Move a cursor, returning the key / value pair found.
Supports current, first, last, next, next-dup, next-nodup,
prev, prev-nodup."
  (declare (type pointer-void cursor)
       (type buffer-stream key-buffer-stream value-buffer-stream)
       (type boolean current first last next next-dup next-nodup prev 
         prev-nodup dirty-read read-uncommitted))
  (loop 
   for key-length fixnum = (buffer-stream-length key-buffer-stream)
   for value-length fixnum = (buffer-stream-length value-buffer-stream)
   do
   (multiple-value-bind (errno ret-key-size result-size)
       (%db-cursor-get-key-buffered cursor 
                    (buffer-stream-buffer key-buffer-stream)
                    0 key-length
                    (buffer-stream-buffer value-buffer-stream)
                    0 value-length
                    (flags :current current
                       :first first
                       :last last
                       :next next
                       :next-dup next-dup
                       :next-nodup next-nodup
                       :prev prev
                       :prev-nodup prev-nodup
                       :dirty-read (or dirty-read read-uncommitted)))
     (declare (type fixnum errno ret-key-size result-size))
     (cond 
       ((= errno 0)
    (setf (buffer-stream-size key-buffer-stream) ret-key-size)
    (setf (buffer-stream-size value-buffer-stream) result-size)
    (return-from db-cursor-move-buffered 
      (the (values buffer-stream buffer-stream)
        (values key-buffer-stream value-buffer-stream))))
       ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
    (return-from db-cursor-move-buffered (values nil nil)))
       ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
    (throw 'transaction *current-transaction*))
       ((or (> result-size value-length) (> ret-key-size key-length))
    (resize-buffer-stream-no-copy value-buffer-stream result-size)
    (resize-buffer-stream-no-copy key-buffer-stream ret-key-size))
       (t (error 'bdb-db-error :errno errno))))))

;; set, set-range: sets key
(defun db-cursor-set-buffered (cursor key-buffer-stream value-buffer-stream
                   &key set set-range dirty-read read-uncommitted)
  "Move a cursor to a key, returning the key / value pair
found.  Supports set and set-range."
  (declare (type pointer-void cursor)
       (type buffer-stream key-buffer-stream value-buffer-stream)
       (type boolean set set-range dirty-read read-uncommitted))
  (loop 
   for key-length fixnum = (buffer-stream-length key-buffer-stream)
   for value-length fixnum = (buffer-stream-length value-buffer-stream)
   do
   (multiple-value-bind (errno ret-key-size result-size)
       (%db-cursor-get-key-buffered cursor 
                    (buffer-stream-buffer key-buffer-stream)
                    (buffer-stream-size key-buffer-stream)
                    key-length
                    (buffer-stream-buffer value-buffer-stream)
                    0 value-length
                    (flags :set set
                       :set-range set-range
                       :dirty-read (or dirty-read read-uncommitted)))
     (declare (type fixnum errno ret-key-size result-size))
     (cond 
       ((= errno 0)
    (setf (buffer-stream-size key-buffer-stream) ret-key-size)
    (setf (buffer-stream-size value-buffer-stream) result-size)
    (return-from db-cursor-set-buffered 
      (the (values buffer-stream buffer-stream)
        (values key-buffer-stream value-buffer-stream))))
       ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
    (return-from db-cursor-set-buffered (values nil nil)))
       ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
    (throw 'transaction *current-transaction*))
       ((or (> result-size value-length) (> ret-key-size key-length))
    (resize-buffer-stream-no-copy value-buffer-stream result-size)
    (resize-buffer-stream key-buffer-stream ret-key-size))
       (t (error 'bdb-db-error :errno errno))))))

;; get-both, get-both-range : sets both
(defun db-cursor-get-both-buffered (cursor key-buffer-stream 
                    value-buffer-stream
                    &key get-both get-both-range dirty-read read-uncommitted)
  "Move a cursor to a key / value pair, returning the key /
value pair found.  Supports get-both and get-both-range."
  (declare (type pointer-void cursor)
       (type buffer-stream key-buffer-stream value-buffer-stream)
       (type boolean get-both get-both-range dirty-read read-uncommitted))
  (loop 
   for key-length fixnum = (buffer-stream-length key-buffer-stream)
   for value-length fixnum = (buffer-stream-length value-buffer-stream)
   do
   (multiple-value-bind (errno ret-key-size result-size)
       (%db-cursor-get-key-buffered cursor 
                    (buffer-stream-buffer key-buffer-stream)
                    (buffer-stream-size	key-buffer-stream)
                    key-length
                    (buffer-stream-buffer value-buffer-stream)
                    (buffer-stream-size	value-buffer-stream)
                    value-length
                    (flags :get-both get-both
                       :get-both-range get-both-range
                       :dirty-read (or dirty-read read-uncommitted)))
     (declare (type fixnum errno ret-key-size result-size))
     (cond 
       ((= errno 0)
    (setf (buffer-stream-size key-buffer-stream) ret-key-size)
    (setf (buffer-stream-size value-buffer-stream) result-size)
    (return-from db-cursor-get-both-buffered 
      (the (values buffer-stream buffer-stream)
        (values key-buffer-stream value-buffer-stream))))
       ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
    (return-from db-cursor-get-both-buffered (values nil nil)))
       ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
    (throw 'transaction *current-transaction*))
       ((or (> result-size value-length) (> ret-key-size key-length))
    (resize-buffer-stream key-buffer-stream ret-key-size)
    (resize-buffer-stream value-buffer-stream result-size))
       (t (error 'bdb-db-error :errno errno))))))
