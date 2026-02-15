;;; writebatch.lisp --- RocksDB Writebatches

;; RocksDB Writebatch Lisp FFI

;;; Commentary:

;; ref: https://github.com/facebook/rocksdb/wiki/Write-Batch-With-Index

;;; Code:
(in-package :rocksdb)

;;; Types
(define-alien-type rocksdb-put-function
    (function void
        (* t)
        (array unsigned-char)
      size-t
      (array unsigned-char)
      size-t))

(define-alien-type rocksdb-delete-function
    (function void
        (* t)
        (array unsigned-char)
      size-t))

(define-alien-type rocksdb-put-cf-function
    (function void
        (* t)
        (unsigned 32)
      (array unsigned-char)
      size-t
      (array unsigned-char)
      size-t))

(define-alien-type rocksdb-delete-cf-function
    (function void
        (* t)
        (unsigned 32)
      (array unsigned-char)
      size-t))

(define-alien-type rocksdb-merge-cf-function
    (function void
        (* t)
        (unsigned 32)
      (array unsigned-char)
      size-t
      (array unsigned-char)
      size-t))

(define-alien-type rocksdb-get-ts-size-function
    (function size-t
        (* t)
        (unsigned 32)))

;;; Alien Functions
(defar rocksdb-writebatch-create (* rocksdb-writebatch))
(defar rocksdb-writebatch-create-from (* rocksdb-writebatch)
  (rep c-string)
  (size size-t))

(defar rocksdb-writebatch-create-with-params (* rocksdb-writebatch)
  (reserved-bytes size-t)
  (max-bytes size-t)
  (protection-bytes-per-key size-t)
  (default-cf-ts-sz size-t))

(defar rocksdb-writebatch-destroy void (batch (* rocksdb-writebatch)))

(defar rocksdb-writebatch-clear void (b (* rocksdb-writebatch)))
(defar rocksdb-writebatch-count int (b (* rocksdb-writebatch)))

(defar rocksdb-writebatch-put-log-data void
  (batch (* rocksdb-writebatch))
  (blob (array unsigned-char))
  (len size-t))

(defar rocksdb-writebatch-iterate void
  (batch (* rocksdb-writebatch))
  (state (* t))
  (put (* rocksdb-put-function))
  (deleted (* rocksdb-delete-function)))`

(defar rocksdb-writebatch-iterate-cf void
  (batch (* rocksdb-writebatch))
  (state (* t))
  (put-cf (* rocksdb-put-cf-function))
  (deleted-cf (* rocksdb-delete-cf-function))
  (merge-cf (* rocksdb-merge-cf-function)))

(defar rocksdb-writebatch-data (array unsigned-char)
  (batch (* rocksdb-writebatch))
  (size (* size-t)))

(defar rocksdb-writebatch-set-savepoint void
  (batch (* rocksdb-writebatch)))

(def-with-errptr rocksdb-writebatch-rollback-to-savepoint void
  (batch (* rocksdb-writebatch)))

(def-with-errptr rocksdb-writebatch-pop-savepoint void
  (batch (* rocksdb-writebatch)))

(def-with-errptr rocksdb-writebatch-update-timestamps void
  (batch (* rocksdb-writebatch))
  (ts (array unsigned-char))
  (tslen size-t)
  (state (* t))
  (get-ts-size (* rocksdb-get-ts-size-function)))

;; put
(defar rocksdb-writebatch-put void
  (batch (* rocksdb-writebatch))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(defar rocksdb-writebatch-put-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(defar rocksdb-writebatch-put-cf-with-ts void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (ts (array unsigned-char))
  (tslen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(defar rocksdb-writebatch-putv void
  (batch (* rocksdb-writebatch))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

(defar rocksdb-writebatch-putv-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

;; delete
(defar rocksdb-writebatch-delete void
  (batch (* rocksdb-writebatch))
  (key (array unsigned-char))
  (klen size-t))

(defar rocksdb-writebatch-singledelete void
  (batch (* rocksdb-writebatch))
  (key (array unsigned-char))
  (klen size-t))

(defar rocksdb-writebatch-delete-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t))

(defar rocksdb-writebatch-singledelete-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t))

(defar rocksdb-writebatch-singledelete-cf-with-ts void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (ts (array unsigned-char))
  (tslen size-t))

(defar rocksdb-writebatch-delete-cf-with-ts void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (ts (array unsigned-char))
  (tslen size-t))

(defar rocksdb-writebatch-deletev void
  (batch (* rocksdb-writebatch))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t)))

(defar rocksdb-writebatch-deletev-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t)))

(defar rocksdb-writebatch-delete-range void
  (batch (* rocksdb-writebatch))
  (start-key (array unsigned-char))
  (start-key-len size-t)
  (end-key (array unsigned-char))
  (end-key-len size-t))

(defar rocksdb-writebatch-delete-range-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (start-key (array unsigned-char))
  (start-key-len size-t)
  (end-key (array unsigned-char))
  (end-key-len size-t))

(defar rocksdb-writebatch-delete-rangev void
  (batch (* rocksdb-writebatch))
  (num-keys int)
  (start-keys-list (array (array unsigned-char)))
  (start-keys-list-sizes (array size-t))
  (end-keys-list (array (array unsigned-char)))
  (end-keys-list-sizes (array size-t)))

(defar rocksdb-writebatch-delete-rangev-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (start-keys-list (array (array unsigned-char)))
  (start-keys-list-sizes (array size-t))
  (end-keys-list (array (array unsigned-char)))
  (end-keys-list-sizes (array size-t)))

;; merge
(defar rocksdb-writebatch-merge void
  (batch (* rocksdb-writebatch))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(defar rocksdb-writebatch-merge-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(defar rocksdb-writebatch-mergev void
  (batch (* rocksdb-writebatch))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

(defar rocksdb-writebatch-mergev-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

;;; with-index (wi)
(defar rocksdb-writebatch-wi-create (* rocksdb-writebatch-wi)
  (reserved-bytes size-t)
  (overwrite-keys unsigned-char))

(defar rocksdb-writebatch-wi-create-from (* rocksdb-writebatch-wi)
  (rep (array unsigned-char))
  (size size-t))

(defar rocksdb-writebatch-wi-create-with-params (* rocksdb-writebatch-wi)
  (backup-index-comparator (* rocksdb-comparator))
  (reserved-bytes size-t)
  (overwrite-key unsigned-char)
  (max-bytes size-t)
  (protection-bytes-per-key size-t))

(defar rocksdb-writebatch-wi-destroy void
  (batch (* rocksdb-writebatch-wi)))

(defar rocksdb-writebatch-wi-clear void
  (batch (* rocksdb-writebatch-wi)))

(defar rocksdb-writebatch-wi-count int
  (batch (* rocksdb-writebatch-wi)))

(defar rocksdb-writebatch-wi-put-log-data void
  (batch (* rocksdb-writebatch-wi))
  (blob (array unsigned-char))
  (len size-t))

(defar rocksdb-writebatch-wi-iterate void
  (batch (* rocksdb-writebatch-wi))
  (state (* t))
  (put (* rocksdb-put-function))
  (deleted (* rocksdb-delete-function)))

(defar rocksdb-writebatch-wi-data (array unsigned-char)
  (batch (* rocksdb-writebatch-wi))
  (size (* size-t)))

(defar rocksdb-writebatch-wi-set-save-point void
  (batch (* rocksdb-writebatch-wi)))

(def-with-errptr rocksdb-writebatch-wi-rollback-to-save-point void
  (batch (* rocksdb-writebatch-wi)))

(def-with-errptr rocksdb-writebatch-wi-get-from-batch (* unsigned-char)
  (batch (* rocksdb-writebatch-wi))
  (opts (* rocksdb-options))
  (key (array unsigned-char))
  (keylen size-t)
  (vallen (* size-t)))

(def-with-errptr rocksdb-writebatch-wi-get-from-batch-cf (* unsigned-char)
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (opts (* rocksdb-options))
  (key (array unsigned-char))
  (keylen size-t)
  (vallen (* size-t)))

(def-with-errptr rocksdb-writebatch-wi-get-from-batch-and-db (* unsigned-char)
  (batch (* rocksdb-writebatch-wi))
  (db (* rocksdb))
  (readopts (* rocksdb-readoptions))
  (key (array unsigned-char))
  (keylen size-t)
  (vallen (* size-t)))

(def-with-errptr rocksdb-writebatch-wi-get-from-batch-and-db-cf (* unsigned-char)
  (batch (* rocksdb-writebatch-wi))
  (db (* rocksdb))
  (readopts (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (keylen size-t)
  (vallen (* size-t)))

(def-with-errptr rocksdb-write-writebatch-wi void
  (db (* rocksdb))
  (wopts (* rocksdb-writeoptions))
  (wbwi (* rocksdb-writebatch-wi)))

(defar rocksdb-writebatch-wi-create-iterator-with-base (* rocksdb-iterator)
  (wbwi (* rocksdb-writebatch-wi))
  (base-iterator (* rocksdb-iterator)))

(defar rocksdb-writebatch-wi-create-iterator-with-base-readopts (* rocksdb-iterator)
  (wbwi (* rocksdb-writebatch-wi))
  (base-iterator (* rocksdb-iterator))
  (options (* rocksdb-readoptions)))

(defar rocksdb-writebatch-wi-create-iterator-with-base-cf (* rocksdb-iterator)
  (wbwi (* rocksdb-writebatch-wi))
  (base-iterator (* rocksdb-iterator))
  (cf (* rocksdb-column-family-handle)))

(defar rocksdb-writebatch-wi-create-iterator-with-base-cf-readopts (* rocksdb-iterator)
  (wbwi (* rocksdb-writebatch-wi))
  (base-iterator (* rocksdb-iterator))
  (cf (* rocksdb-column-family-handle))
  (options (* rocksdb-readoptions)))

(def-with-errptr rocksdb-writebatch-wi-update-timestamps void
  (wbwi (* rocksdb-writebatch-wi))
  (ts (array unsigned-char))
  (tslen size-t)
  (state (* t))
  (get-ts-size (* rocksdb-get-ts-size-function)))

(defar rocksdb-writebatch-wi-put void
  (batch (* rocksdb-writebatch-wi))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(defar rocksdb-writebatch-wi-put-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (key (* unsigned-char))
  (klen size-t)
  (val (* unsigned-char))
  (vlen size-t))

(defar rocksdb-writebatch-wi-putv void
  (batch (* rocksdb-writebatch-wi))
  (num-keys int)
  (keys-list (* (* unsigned-char)))
  (keys-list-sizes (array size-t))
  (num-values int)
  (values-list (* (* unsigned-char)))
  (values-list-sizes (array size-t)))

(defar rocksdb-writebatch-wi-putv-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

(defar rocksdb-writebatch-wi-merge void
  (batch (* rocksdb-writebatch-wi))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(defar rocksdb-writebatch-wi-merge-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(defar rocksdb-writebatch-wi-mergev void
  (batch (* rocksdb-writebatch-wi))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

(defar rocksdb-writebatch-wi-mergev-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

(defar rocksdb-writebatch-wi-delete void
  (batch (* rocksdb-writebatch-wi))
  (key (array unsigned-char))
  (klen size-t))

(defar rocksdb-writebatch-wi-singledelete void
  (batch (* rocksdb-writebatch-wi))
  (key (array unsigned-char))
  (klen size-t))

(defar rocksdb-writebatch-wi-delete-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t))

(defar rocksdb-writebatch-wi-singledelete-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t))

(defar rocksdb-writebatch-wi-deletev void
  (batch (* rocksdb-writebatch-wi))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t)))

(defar rocksdb-writebatch-wi-deletev-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t)))

;;; DO NOT USE
(defar rocksdb-writebatch-wi-delete-range void
  (batch (* rocksdb-writebatch-wi))
  (start-key (array unsigned-char))
  (start-key-len size-t)
  (end-key (array unsigned-char))
  (end-key-len size-t))

(defar rocksdb-writebatch-wi-delete-range-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (start-key (array unsigned-char))
  (start-key-len size-t)
  (end-key (array unsigned-char))
  (end-key-len size-t))

(defar rocksdb-writebatch-wi-delete-rangev void
  (batch (* rocksdb-writebatch-wi))
  (start-keys-list (array (array unsigned-char)))
  (start-keys-list-sizes (array size-t))
  (end-keys-list (array (array unsigned-char)))
  (end-keys-list-sizes (array size-t)))

(defar rocksdb-writebatch-wi-delete-rangev-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (start-keys-list (array (array unsigned-char)))
  (start-keys-list-sizes (array size-t))
  (end-keys-list (array (array unsigned-char)))
  (end-keys-list-sizes (array size-t)))
