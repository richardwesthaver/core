;;; writebatch.lisp --- RocksDB Writebatches

;; RocksDB Writebatch Lisp FFI

;;; Code:
(in-package :rocksdb)

(define-alien-routine rocksdb-writebatch-create (* rocksdb-writebatch))
(define-alien-routine rocksdb-writebatch-create-from (* rocksdb-writebatch)
  (rep c-string)
  (size size-t))

(define-alien-routine rocksdb-writebatch-create-with-params (* rocksdb-writebatch)
  (reserved-bytes size-t)
  (max-bytes size-t)
  (protection-bytes-per-key size-t)
  (default-cf-ts-sz size-t))

(define-alien-routine rocksdb-writebatch-destroy void (batch (* rocksdb-writebatch)))

(define-alien-routine rocksdb-writebatch-clear void (b (* rocksdb-writebatch)))
(define-alien-routine rocksdb-writebatch-count int (b (* rocksdb-writebatch)))

;; put
(define-alien-routine rocksdb-writebatch-put void
  (batch (* rocksdb-writebatch))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(define-alien-routine rocksdb-writebatch-put-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(define-alien-routine rocksdb-writebatch-put-cf-with-ts void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (ts (array unsigned-char))
  (tslen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(define-alien-routine rocksdb-writebatch-putv void
  (batch (* rocksdb-writebatch))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

(define-alien-routine rocksdb-writebatch-putv-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

;; delete
(define-alien-routine rocksdb-writebatch-delete void
  (batch (* rocksdb-writebatch))
  (key (array unsigned-char))
  (klen size-t))

(define-alien-routine rocksdb-writebatch-delete-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t))

(define-alien-routine rocksdb-writebatch-delete-cf-with-ts void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (ts (array unsigned-char))
  (tslen size-t))

;; merge

;; savepoints

;; with-index (wi)

(define-alien-routine rocksdb-writebatch-wi-create (* rocksdb-writebatch-wi)
  (reserved-bytes size-t)
  (overwrite-keys unsigned-char))

(define-alien-routine rocksdb-writebatch-wi-create-from (* rocksdb-writebatch-wi)
  (rep (array unsigned-char))
  (size size-t))

(define-alien-routine rocksdb-writebatch-wi-create-with-params (* rocksdb-writebatch-wi)
  (backup-index-comparator (* rocksdb-comparator))
  (reserved-bytes size-t)
  (overwrite-key unsigned-char)
  (max-bytes size-t)
  (protection-bytes-per-key size-t))
