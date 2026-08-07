;;; rdb/io.lisp --- IO Primitives

;; RDB support for STATIC/BUFFER-STREAM APIs.

;;; Commentary:

;; The functions in this section use the BUFFER-STREAM protocol from the IO
;; system and are used to implement the RocksDB backend for the STORE
;; protocol.

;; The BUFFER slot of every BUFFER-STREAM is a SAP which is filled with a key
;; value before being sent to RocksDB, and set to the corresponding value of a
;; PinnableSlice C struct when retrieving a value for decoding.

;;; Code:
(in-package :rdb)

(defmacro txn-default (dvar)
  `(progn
     (assert (null ,dvar))
     nil))

#+nil
(defun db-get-key-buffered (db kbuf vbuf &key (transaction (txn-default *transaction*)))
  "Get a KV from a DB. The key is encoded in a buffer-stream and on success a
  buffer-stream for decoding the value is returned or NIL if nothing was
  found."
  (declare (buffer-stream kbuf vbuf))
  (loop 
    for key-length fixnum = (buffer-stream-length key-buffer-stream)
    for value-length fixnum = (buffer-stream-length value-buffer-stream)
   do (multiple-value-bind (val result-size)
          (%transaction-get transaction (buffer kbuf) opts
                            (buffer key-buffer-stream)
                            (size key-buffer-stream)
                            e)
     (declare (type fixnum result-size errno))
     (cond 
       ((= errno 0)
    ;(setf (buffer-stream-size key-buffer-stream) ret-key-size)
    (setf (size value-buffer-stream) result-size)
    (return-from db-get-key-buffered 
      (the buffer-stream value-buffer-stream)))
       ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
    (return-from db-get-key-buffered nil))
       ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
    (throw 'transaction transaction))
       ((or (> result-size value-length) (> ret-key-size key-length))
    (resize-buffer-stream-no-copy value-buffer-stream result-size)
    (resize-buffer-stream-no-copy key-buffer-stream ret-key-size))
       (t (error 'bdb-db-error :errno errno))))))
