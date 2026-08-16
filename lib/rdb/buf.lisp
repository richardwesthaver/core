;;; rdb/buf.lisp --- Buffered API

;; RocksDB functions where data values are converted between BUFFER-STREAMs.

;;; Commentary:

;; This file contains the code which most implementations should
;; use. De/Serialization is delayed and can support any Lisp object from
;; BUFFER-STREAMs.

;; Users should always prefer zero-copy/buffered/pinned/slice optimizations
;; and these functions supply appropriate defaults based on that assumption.

;; Timestamps are optional, always buffer-streams.

;; AO <2026-08-15 Sat> Only supports plain ROCKSDB. It is unknown what the
;; impact of grabbing the base-db is.

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

(defmacro set-slice-streams (&body pairs)
  `(values ,@(loop for (k v) on pairs by #'cddr collect `(slice-stream ,v ,k))))

(defun pslice-stream (pslice stream)
  (with-pslice pslice
    (when (> size (buffer-stream-length stream)) (resize-buffer-stream stream size))
    (setf (size stream) size
          (buffer stream) data)))

(defun rdb-key-may-exist (db kbuf &key (opts *default-rocksdb-readoptions*) timestamp cf)
  (declare (buffer-stream kbuf))
  (with-alien ((found boolean)
               (v (* unsigned-char))
               (vlen size-t))
    (and
     (if cf
         (rocksdb-key-may-exist-cf db opts cf (buffer kbuf) (size kbuf) (addr v) (addr vlen)
                                   (when timestamp (buffer timestamp))
                                   (if timestamp (size timestamp) 0)
                                   (addr found))
         (rocksdb-key-may-exist db opts (buffer kbuf) (size kbuf) (addr v) (addr vlen)
                                (when timestamp (buffer timestamp)) (if timestamp (size timestamp) 0)
                                (addr found)))
     found
     (not (zerop vlen))
     (%make-slice-stream v vlen))))

(defun rdb-get-buf (db kbuf vbuf &key (opts *default-rocksdb-readoptions*) cf)
  "Get a key from DB using the buffered RocksDB functions.
Does not support direct timestamps."
  (declare (buffer-stream kbuf vbuf))
  (with-kv-buf* (db kbuf vbuf e :cf cf)
    (repeat 2
      (and
       (if cf
           (rocksdb-get-into-buffer-cf db opts cf %key %ksize %val %vlen e)
           (rocksdb-get-into-buffer db opts %key %ksize %val %vlen e))
       (return-from db-get-buf vbuf)) ;; check that this value is updated..
      (resize-buffer-stream vbuf %vlen))))

(defun rdb-get (db kbuf &key (opts *default-rocksdb-readoptions*) cf)
  "Get a key from DB using the v2 zero-copy RocksDB functions if possible."
  (declare (buffer-stream kbuf))
  (with-key-buf (db kbuf e :cf cf)
    (with-phandle
        (if cf
            (rocksdb-get-pinned-cf-v2 db opts cf %key %ksize e)
            (rocksdb-get-pinned-v2 db opts %key %ksize e))
      ;; not optimal..
      (%make-slice-stream data size))))

(defun trdb-get (db kbuf &key (opts *default-rocksdb-readoptions*) cf)
  (declare (buffer-stream kbuf))
  (with-key-buf (db kbuf e :cf cf)
    (with-pslice
        (if cf
            (rocksdb-transactiondb-get-pinned-cf db opts cf %key %ksize e)
            (rocksdb-transactiondb-get-pinned db opts %key %ksize e))
      ;; not optimal..
      (%make-slice-stream data size))))

(define-db-surrogate db-get rdb-get trdb-get)

(defun rdb-put (db kbuf vbuf &key (opts *default-rocksdb-writeoptions*) cf timestamp)
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

(defun trdb-put (db kbuf vbuf &key (opts *default-rocksdb-writeoptions*) cf)
  (declare (buffer-stream kbuf vbuf))
  (with-kv-buf (db kbuf vbuf e :cf cf)
    (if cf
        (rocksdb-transactiondb-put-cf db opts cf %key %ksize %val %vsize e)
        (rocksdb-transactiondb-put db opts %key %ksize %val %vsize e))))

(define-db-surrogate db-put rdb-put trdb-put)

;; the following two functions are restricted to a SINGLE column family
(defun rdb-multi-get-batch (db kbufs &key (opts *default-rocksdb-readoptions*) cf sorted)
  "Get a list of keys from DB using the batched/pinned RocksDB functions.")

(defun rdb-multi-get-slice (db kbufs &key (opts *default-rocksdb-readoptions*) cf sorted)
  "Get a list of keys from DB using the slice RocksDB function.")

;; generic multi-get functions
(defun rdb-multi-get (db kbufs &key (opts *default-rocksdb-readoptions*) cfs timestamp))

(defun trdb-multi-get (db kbufs &key (opts *default-rocksdb-readoptions*) cfs))

(defun rdb-merge (db kbuf vbuf &key (opts *default-rocksdb-writeoptions*) cf)
  (declare (buffer-stream kbuf vbuf))
  (with-kv-buf (db kbuf vbuf e :cf cf)
    (if cf
        (rocksdb-merge-cf db opts cf %key %ksize %val %vsize e)
        (rocksdb-merge db opts %key %ksize %val %vsize e))))

(defun trdb-merge (db kbuf vbuf &key (opts *default-rocksdb-writeoptions*) cf)
  (declare (buffer-stream kbuf vbuf))
  (with-kv-buf (db kbuf vbuf e :cf cf)
    (if cf
        (rocksdb-transactiondb-merge-cf db opts cf %key %ksize %val %vsize e)
        (rocksdb-transactiondb-merge db opts %key %ksize %val %vsize e))))

(define-db-surrogate db-merge rdb-merge trdb-merge)

(defun rdb-delete (db kbuf &key (opts *default-rocksdb-writeoptions*) cf timestamp)
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


(defun trdb-delete (db kbuf &key (opts *default-rocksdb-writeoptions*) cf)
  (declare (buffer-stream kbuf))
  (with-key-buf (db kbuf e :cf cf)
    (if cf
        (rocksdb-transactiondb-delete-cf db opts cf %key %ksize e)
        (rocksdb-transactiondb-delete db opts %key %ksize e))))

(define-db-surrogate db-delete rdb-delete trdb-delete)

(defun rdb-single-delete (db kbuf &key (opts *default-rocksdb-writeoptions*) cf timestamp)
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

(defun rdb-delete-range (db sbuf ebuf &key (opts *default-rocksdb-writeoptions*) cf)
  "Delete a range of keys from DB starting at SBUF and ending at EBUF."
  (declare (buffer-stream sbuf ebuf))
  (with-key-range (db sbuf ebuf e :cf cf)
    (rocksdb-delete-range-cf db opts cf %skey %ssize %ekey %esize e)))
