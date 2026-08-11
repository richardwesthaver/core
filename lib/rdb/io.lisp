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

(defmacro with-slice ((data size) slice &body body)
  "Eval BODY with the pinnable-slice pointer SLICE destructured into DATA and SIZE values."
  `(multiple-value-bind (,data ,size) (rocksdb::rocksdb-pinnableslice-value ,slice)
     ,@body
     (rocksdb::rocksdb-pinnableslice-destroy ,slice)))

(defun get-key-buffered (kbuf vbuf &key (transaction (txn-default *transaction*)) 
                                        (opts (default-rocksdb-readoptions))
                                        cf)
  "Get a KV from a DB. The key is encoded in a buffer-stream and on success a
buffer-stream for decoding the value is returned or NIL if nothing was found."
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

#+todo
(defun put-buffered (kbuf vbuf
                     &key (transaction (txn-default *current-transaction*))
                          exists-error-p no-dup)
  "Put a key / value pair into a DB.  The pair are encoded
in buffer-streams.  T on success, or nil if the key already
exists and EXISTS-ERROR-P is NIL."
  (declare (type pointer-void db transaction)
       (type buffer-stream key-buffer-stream value-buffer-stream)
       (type boolean exists-error-p))
  (let ((errno 
     (%db-put-buffered db transaction 
               (buffer-stream-buffer key-buffer-stream)
               (buffer-stream-size key-buffer-stream)
               (buffer-stream-buffer value-buffer-stream)
               (buffer-stream-size value-buffer-stream)
               (if no-dup DB_NODUPDATA 0))))
    (declare (type fixnum errno))
    (cond ((= errno 0) t)
      ((and (= errno DB_KEYEXIST) (not exists-error-p))
       nil)
      ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
       (throw 'transaction transaction))
      (t (error 'bdb-db-error :errno errno)))))
