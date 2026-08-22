;;; rdb/buf.lisp --- Buffered API

;; RocksDB functions where data values are converted between BUFFER-STREAMs.

;;; Commentary:

;; This file contains the code which most implementations should
;; use. De/Serialization is delayed and can support any Lisp object from
;; BUFFER-STREAMs.

;; Users should always prefer zero-copy/buffered/pinned/slice optimizations
;; and these functions supply appropriate defaults based on that assumption.

;; Timestamps are optional, always buffer-streams.

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

;; We currently don't support passing intermediate buffers (for value
;; encoding), but will likely need to add those. Trivial to implement with the
;; macros.

;;; Code:
(in-package :rdb)

(defun key-exists (db kbuf &key (opts (default-rocksdb-readoptions)) timestamp cf)
  "Return a BUFFER-STREAM if key KBUF exists in DB, else return nil."
  (declare (buffer-stream kbuf))
  (with-alien ((may-exist boolean)
               (v (* unsigned-char))
               (vlen size-t))
    (and
     (if cf
         (rocksdb-key-may-exist-cf db opts cf (buffer kbuf) (size kbuf) (addr v) (addr vlen)
                                   (when timestamp (buffer timestamp))
                                   (if timestamp (size timestamp) 0)
                                   (addr may-exist))
         (rocksdb-key-may-exist db opts (buffer kbuf) (size kbuf) (addr v) (addr vlen)
                                (when timestamp (buffer timestamp)) (if timestamp (size timestamp) 0)
                                (addr may-exist)))
     may-exist
     (not (zerop vlen))
     (%make-slice-stream v vlen))))

(defmacro unless-key-exists ((key db &key cf timestamp)
                             &body body)
  `(unless (key-exists ,db ,key :timestamp ,timestamp :cf ,cf)
     ,@body))

(defun rdb-get-buf (db kbuf vbuf &key (opts (default-rocksdb-readoptions)) cf)
  "Get a key from DB using the buffered RocksDB functions.
Does not support direct timestamps."
  (declare (buffer-stream kbuf vbuf))
  (with-kv-buf* (db kbuf vbuf e :cf cf)
    (repeat 2
      (and
       (if cf
           (rocksdb-get-into-buffer-cf db opts cf %key %ksize %val %vlen e)
           (rocksdb-get-into-buffer db opts %key %ksize %val %vlen e))
       (return-from rdb-get-buf vbuf)) ; check that this value is updated..
      (setf vbuf (resize-buffer-stream vbuf %vlen)))))

(defun rdb-get (db kbuf &key (opts (default-rocksdb-readoptions)) cf)
  "Get a key from DB using the v2 zero-copy RocksDB functions if possible."
  (declare (buffer-stream kbuf))
  (with-key-buf (db kbuf e :cf cf)
    (with-phandle
        (if cf
            (rocksdb-get-pinned-cf-v2 db opts cf %key %ksize e)
            (rocksdb-get-pinned-v2 db opts %key %ksize e))
      ;; not optimal..
      (%make-slice-stream data size))))

(defun trdb-get (db kbuf &key (opts (default-rocksdb-readoptions)) cf)
  (declare (buffer-stream kbuf))
  (with-kbuf (e kbuf)
    (with-pslice
        (if cf
            (rocksdb-transactiondb-get-pinned-cf db opts cf %key %ksize e)
            (rocksdb-transactiondb-get-pinned db opts %key %ksize e))
      ;; not optimal..
      (%make-slice-stream data size))))

(define-db-surrogate db-get rdb-get trdb-get)

(defun rdb-put (db kbuf vbuf &key (opts (default-rocksdb-writeoptions)) cf timestamp)
  (declare (buffer-stream kbuf vbuf))
  (with-kv-buf (db kbuf vbuf e :cf cf)
  (if timestamp
      (with-ts-buf timestamp
        (if cf
            (rocksdb-put-cf-with-ts db opts cf %key %ksize %ts %tslen %val %vsize e)
            (rocksdb-put-with-ts db opts %key %ksize %ts %tslen %val %vsize e)))
      (if cf
          (rocksdb-put-cf db opts cf %key %ksize %val %vsize e)
          (rocksdb-put db opts %key %ksize %val %vsize e)))))

(defun trdb-put (db kbuf vbuf &key (opts (default-rocksdb-writeoptions)) cf)
  (declare (buffer-stream kbuf vbuf))
  (with-kv-buf (db kbuf vbuf e :cf cf)
    (if cf
        (rocksdb-transactiondb-put-cf db opts cf %key %ksize %val %vsize e)
        (rocksdb-transactiondb-put db opts %key %ksize %val %vsize e))))

(define-db-surrogate db-put rdb-put trdb-put)

(defun rdb-insert (db kbuf vbuf &key (opts (default-rocksdb-writeoptions)) cf timestamp)
  (unless-key-exists (kbuf db :cf cf :timestamp timestamp)
    (rdb-put db kbuf vbuf :opts opts :cf cf :timestamp timestamp)))

(defun trdb-insert (db kbuf vbuf &key (opts (default-rocksdb-writeoptions)) cf)
  (unless-key-exists (kbuf db :cf cf)
    (trdb-put db kbuf vbuf :opts opts :cf cf)))

(define-db-surrogate db-insert rdb-insert trdb-insert)

;; the following two functions are restricted to a SINGLE column family
(defun rdb-multi-get-batch (db keys klen &key (opts (default-rocksdb-readoptions)) cf sorted)
  "Get a list of keys from DB using the batched/pinned RocksDB functions. Unlike
most other functions KEYS is assumed to be an alien (* ROCKSDB-SLICE)."
  (rocksdb-batched-multi-get-pinned-cf db opts cf klen keys sorted)
  keys)

(defun rdb-multi-get-slice (db keys klen &key (opts (default-rocksdb-readoptions)) cf sorted)
  "Get a list of keys from DB using the slice RocksDB function. KEYS is assumed
to be an alien (* ROCKSDB-SLICE)."
  (with-val-bufs (klen e)
    (rocksdb-batched-multi-get-cf-slice db opts cf klen keys %vals e sorted)
    (values %vals e)))

;; generic multi-get functions
(defun rdb-multi-get (db kbufs &key (opts (default-rocksdb-readoptions)) cfs timestamps)
  (with-key-bufs (kbufs e)
    (if timestamps
        (with-ts-bufs timestamps
          (if cfs
              (rocksdb-multi-get-cf-with-ts 
               db opts cfs 
               %klen %keys %ksizes %vals %vsizes %ts %tsizes e)
              (rocksdb-multi-get-with-ts db opts %klen %keys %ksizes %vals %vsizes %ts %tsizes e)))
        (if cfs
            (rocksdb-multi-get-cf db opts cfs %klen  %keys %ksizes %vals %vsizes e)
            (rocksdb-multi-get db opts %klen %keys %ksizes %vals %vsizes e)))))

(defun trdb-multi-get (db kbufs &key (opts (default-rocksdb-readoptions)) cfs)
  (with-key-bufs (kbufs e)
    (rocksdb-transactiondb-multi-get-cf db opts cfs %klen %keys %ksizes %vals %vsizes e)))

(define-db-surrogate db-multi-get rdb-multi-get trdb-multi-get)

(defun rdb-merge (db kbuf vbuf &key (opts (default-rocksdb-writeoptions)) cf)
  (declare (buffer-stream kbuf vbuf))
  (with-kv-buf (db kbuf vbuf e :cf cf)
    (if cf
        (rocksdb-merge-cf db opts cf %key %ksize %val %vsize e)
        (rocksdb-merge db opts %key %ksize %val %vsize e))))

(defun trdb-merge (db kbuf vbuf &key (opts (default-rocksdb-writeoptions)) cf)
  (declare (buffer-stream kbuf vbuf))
  (with-kv-buf (db kbuf vbuf e :cf cf)
    (if cf
        (rocksdb-transactiondb-merge-cf db opts cf %key %ksize %val %vsize e)
        (rocksdb-transactiondb-merge db opts %key %ksize %val %vsize e))))

(define-db-surrogate db-merge rdb-merge trdb-merge)

(defun rdb-delete (db kbuf &key (opts (default-rocksdb-writeoptions)) cf timestamp)
  "Delete a key from DB."
  (declare (buffer-stream kbuf))
  (with-key-buf (db kbuf e :cf cf)
  (if timestamp
      (with-ts-buf timestamp
        (if cf
            (rocksdb-delete-cf-with-ts db opts cf %key %ksize %ts %tslen e)
            (rocksdb-delete-with-ts db opts %key %ksize %ts %tslen e)))
      (if cf
          (rocksdb-delete-cf db opts cf %key %ksize e)
          (rocksdb-delete db opts %key %ksize e)))))

(defun trdb-delete (db kbuf &key (opts (default-rocksdb-writeoptions)) cf)
  (declare (buffer-stream kbuf))
  (with-key-buf (db kbuf e :cf cf)
    (if cf
        (rocksdb-transactiondb-delete-cf db opts cf %key %ksize e)
        (rocksdb-transactiondb-delete db opts %key %ksize e))))

(define-db-surrogate db-delete rdb-delete trdb-delete)

(defun rdb-single-delete (db kbuf &key (opts (default-rocksdb-writeoptions)) cf timestamp)
  "Single Delete a key from DB."
  (declare (buffer-stream kbuf))
  (with-key-buf (db kbuf e :cf cf)
  (if timestamp
      (with-ts-buf timestamp
        (if cf
            (rocksdb-singledelete-cf-with-ts db opts cf %key %ksize %ts %tslen e)
            (rocksdb-singledelete-with-ts db opts %key %ksize %ts %tslen e)))
      (if cf
          (rocksdb-singledelete-cf db opts cf %key %ksize e)
          (rocksdb-singledelete db opts %key %ksize e)))))

(defun rdb-delete-range (db sbuf ebuf &key (opts (default-rocksdb-writeoptions)) cf)
  "Delete a range of keys from DB starting at SBUF and ending at EBUF."
  (declare (buffer-stream sbuf ebuf))
  (with-key-range (db sbuf ebuf e :cf cf)
    (rocksdb-delete-range-cf db opts cf %skey %ssize %ekey %esize e)))
