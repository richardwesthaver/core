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

(defmacro with-slice ((data size) slice &body body)
  "Eval BODY with the pinnable-slice pointer SLICE destructured into DATA and SIZE values."
  `(multiple-value-bind (,data ,size) (rocksdb::rocksdb-pinnableslice-value ,slice)
     ,@body
     (rocksdb::rocksdb-pinnableslice-destroy ,slice)))
