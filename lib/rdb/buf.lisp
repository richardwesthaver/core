;;; rdb/buf.lisp --- Buffered API

;; RocksDB functions where data values are converted between BUFFER-STREAMs.

;;; Commentary:

;; This file contains the code which most implementations should
;; use. De/Serialization is delayed and can support any Lisp object from
;; BUFFER-STREAMs.

;; Users should always prefer zero-copy/buffered/pinned/slice optimizations
;; and these functions supply appropriate defaults based on that assumption.

;; Timestamps are optional, always buffer-streams.

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

(defun db-key-may-exist (db kbuf &key (opts *default-rocksdb-readoptions*) timestamp cf)
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

(defun db-get-buf (db kbuf vbuf &key (opts *default-rocksdb-readoptions*) cf)
  "Get a key from DB using the buffered RocksDB functions. Does not support timestamps."
  (declare (buffer-stream kbuf vbuf)))

(defun db-get (db kbuf &key (opts *default-rocksdb-readoptions*) cf timestamp)
  "Get a key from DB using the v2 zero-copy RocksDB functions if possible."
  (declare (buffer-stream kbuf)))

(defun db-put (db kbuf vbuf &key (opts *default-rocksdb-writeoptions*) cf timestamp)
  (declare (buffer-stream kbuf vbuf)))

(defun db-multi-get (db kbufs &key (opts *default-rocksdb-readoptions*) cf sorted)
  "Get a list of keys from DB using the batched/pinned RocksDB functions.")

(defun db-merge (db kbuf vbuf &key (opts *default-rocksdb-readoptions*) cf)
  (declare (buffer-stream kbuf vbuf)))

(defun db-delete (db kbuf &key (opts *default-rocksdb-readoptions*) cf timestamp)
  "Delete a key from DB."
  (declare (buffer-stream kbuf)))

(defun db-delete-range (db sbuf ebuf &key (opts *default-rocksdb-readoptions*) cf timestamp)
  "Delete a range of keys from DB starting at SBUF and ending at EBUF."
  (declare (buffer-stream sbuf ebuf)))
