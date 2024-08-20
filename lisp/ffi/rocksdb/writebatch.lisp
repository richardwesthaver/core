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

(define-alien-routine rocksdb-writebatch-put-log-data void
  (batch (* rocksdb-writebatch))
  (blob (array unsigned-char))
  (len size-t))

(define-alien-routine rocksdb-writebatch-iterate void
  (batch (* rocksdb-writebatch))
  (state (* t))
  (put (* t)) ;; function
  (deleted (* t))) ;; function

(define-alien-routine rocksdb-writebatch-iterate-cf void
  (batch (* rocksdb-writebatch))
  (state (* t))
  (put-cf (* t)) ;; function
  (deleted-cf (* t)) ;; function
  (merge-cf (* t))) ;; function

(define-alien-routine rocksdb-writebatch-data (array unsigned-char)
  (batch (* rocksdb-writebatch))
  (size (* size-t)))

(define-alien-routine rocksdb-writebatch-set-save-point void
  (batch (* rocksdb-writebatch)))

(def-with-errptr rocksdb-writebatch-rollback-to-save-point void
  (batch (* rocksdb-writebatch)))

(def-with-errptr rocksdb-writebatch-pop-save-point void
  (batch (* rocksdb-writebatch)))

(def-with-errptr rocksdb-writebatch-update-timestamps void
  (batch (* rocksdb-writebatch))
  (ts (array unsigned-char))
  (tslen size-t)
  (state (* t))
  (get-ts-size (* t))) ;; function

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

(define-alien-routine rocksdb-writebatch-singledelete void
  (batch (* rocksdb-writebatch))
  (key (array unsigned-char))
  (klen size-t))

(define-alien-routine rocksdb-writebatch-delete-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t))

(define-alien-routine rocksdb-writebatch-singledelete-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t))

(define-alien-routine rocksdb-writebatch-singledelete-cf-with-ts void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (ts (array unsigned-char))
  (tslen size-t))

(define-alien-routine rocksdb-writebatch-delete-cf-with-ts void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (ts (array unsigned-char))
  (tslen size-t))

(define-alien-routine rocksdb-writebatch-deletev void
  (batch (* rocksdb-writebatch))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t)))

(define-alien-routine rocksdb-writebatch-deletev-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t)))

(define-alien-routine rocksdb-writebatch-delete-range void
  (batch (* rocksdb-writebatch))
  (start-key (array unsigned-char))
  (start-key-len size-t)
  (end-key (array unsigned-char))
  (end-key-len size-t))

(define-alien-routine rocksdb-writebatch-delete-range-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (start-key (array unsigned-char))
  (start-key-len size-t)
  (end-key (array unsigned-char))
  (end-key-len size-t))

(define-alien-routine rocksdb-writebatch-delete-rangev void
  (batch (* rocksdb-writebatch))
  (num-keys int)
  (start-keys-list (array (array unsigned-char)))
  (start-keys-list-sizes (array size-t))
  (end-keys-list (array (array unsigned-char)))
  (end-keys-list-sizes (array size-t)))

(define-alien-routine rocksdb-writebatch-delete-rangev-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (start-keys-list (array (array unsigned-char)))
  (start-keys-list-sizes (array size-t))
  (end-keys-list (array (array unsigned-char)))
  (end-keys-list-sizes (array size-t)))

;; merge
(define-alien-routine rocksdb-writebatch-merge void
  (batch (* rocksdb-writebatch))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(define-alien-routine rocksdb-writebatch-merge-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(define-alien-routine rocksdb-writebatch-mergev void
  (batch (* rocksdb-writebatch))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

(define-alien-routine rocksdb-writebatch-mergev-cf void
  (batch (* rocksdb-writebatch))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))
  
;;; with-index (wi)
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

(define-alien-routine rocksdb-writebatch-wi-destroy void
  (batch (* rocksdb-writebatch-wi)))

(define-alien-routine rocksdb-writebatch-wi-clear void
  (batch (* rocksdb-writebatch-wi)))

(define-alien-routine rocksdb-writebatch-wi-count void
  (batch (* rocksdb-writebatch-wi)))

(define-alien-routine rocksdb-writebatch-wi-put-log-data void
  (batch (* rocksdb-writebatch-wi))
  (blob (array unsigned-char))
  (len size-t))

(define-alien-routine rocksdb-writebatch-wi-iterate void
  (batch (* rocksdb-writebatch-wi))
  (state (* t))
  (put (* t)) ;; function
  (deleted (* t)) ;; function
  )

(define-alien-routine rocksdb-writebatch-wi-data (array unsigned-char)
  (batch (* rocksdb-writebatch-wi))
  (size (* size-t)))

(define-alien-routine rocksdb-writebatch-wi-set-save-point void
  (batch (* rocksdb-writebatch-wi)))

(def-with-errptr rocksdb-writebatch-wi-rollback-to-save-point void
  (batch (* rocksdb-writebatch-wi)))

(def-with-errptr rocksdb-writebatch-wi-get-from-batch (array unsigned-char)
  (batch (* rocksdb-writebatch-wi))
  (opts (* rocksdb-options))
  (key (array unsigned-char))
  (keylen size-t)
  (vallen (* size-t)))

(def-with-errptr rocksdb-writebatch-wi-get-from-batch-cf (array unsigned-char)
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (opts (* rocksdb-options))
  (key (array unsigned-char))
  (keylen size-t)
  (vallen (* size-t)))

(def-with-errptr rocksdb-writebatch-wi-get-from-batch-and-db (array unsigned-char)
  (batch (* rocksdb-writebatch-wi))
  (db (* rocksdb))
  (readopts (* rocksdb-readoptions))
  (key (array unsigned-char))
  (keylen size-t)
  (vallen (* size-t)))

(def-with-errptr rocksdb-writebatch-wi-get-from-batch-and-db-cf (array unsigned-char)
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

(define-alien-routine rocksdb-writebatch-wi-create-iterator-with-base (* rocksdb-iterator)
  (wbwi (* rocksdb-writebatch-wi))
  (base-iterator (* rocksdb-iterator)))

(define-alien-routine rocksdb-writebatch-wi-create-iterator-with-base-cf (* rocksdb-iterator)
  (wbwi (* rocksdb-writebatch-wi))
  (base-iterator (* rocksdb-iterator))
  (cf (* rocksdb-column-family-handle)))

(def-with-errptr rocksdb-writebatch-wi-update-timestamps void
  (wbwi (* rocksdb-writebatch-wi))
  (ts (array unsigned-char))
  (tslen size-t)
  (state (* t))
  (get-ts-size (* t))) ;; function

(define-alien-routine rocksdb-writebatch-wi-put void
  (batch (* rocksdb-writebatch-wi))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(define-alien-routine rocksdb-writebatch-wi-put-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(define-alien-routine rocksdb-writebatch-wi-putv void
  (batch (* rocksdb-writebatch-wi))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

(define-alien-routine rocksdb-writebatch-wi-putv-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

(define-alien-routine rocksdb-writebatch-wi-merge void
  (batch (* rocksdb-writebatch-wi))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(define-alien-routine rocksdb-writebatch-wi-merge-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t)
  (val (array unsigned-char))
  (vlen size-t))

(define-alien-routine rocksdb-writebatch-wi-mergev void
  (batch (* rocksdb-writebatch-wi))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

(define-alien-routine rocksdb-writebatch-wi-mergev-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t))
  (num-values int)
  (values-list (array (array unsigned-char)))
  (values-list-sizes (array size-t)))

(define-alien-routine rocksdb-writebatch-wi-delete void
  (batch (* rocksdb-writebatch-wi))
  (key (array unsigned-char))
  (klen size-t))

(define-alien-routine rocksdb-writebatch-wi-singledelete void
  (batch (* rocksdb-writebatch-wi))
  (key (array unsigned-char))
  (klen size-t))

(define-alien-routine rocksdb-writebatch-wi-delete-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t))

(define-alien-routine rocksdb-writebatch-wi-singledelete-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (key (array unsigned-char))
  (klen size-t))

(define-alien-routine rocksdb-writebatch-wi-deletev void
  (batch (* rocksdb-writebatch-wi))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t)))

(define-alien-routine rocksdb-writebatch-wi-deletev-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (num-keys int)
  (keys-list (array (array unsigned-char)))
  (keys-list-sizes (array size-t)))

;;; DO NOT USE
(define-alien-routine rocksdb-writebatch-wi-delete-range void
  (batch (* rocksdb-writebatch-wi))
  (start-key (array unsigned-char))
  (start-key-len size-t)
  (end-key (array unsigned-char))
  (end-key-len size-t))

(define-alien-routine rocksdb-writebatch-wi-delete-range-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (start-key (array unsigned-char))
  (start-key-len size-t)
  (end-key (array unsigned-char))
  (end-key-len size-t))

(define-alien-routine rocksdb-writebatch-wi-delete-rangev void
  (batch (* rocksdb-writebatch-wi))
  (start-keys-list (array (array unsigned-char)))
  (start-keys-list-sizes (array size-t))
  (end-keys-list (array (array unsigned-char)))
  (end-keys-list-sizes (array size-t)))

(define-alien-routine rocksdb-writebatch-wi-delete-rangev-cf void
  (batch (* rocksdb-writebatch-wi))
  (cf (* rocksdb-column-family-handle))
  (start-keys-list (array (array unsigned-char)))
  (start-keys-list-sizes (array size-t))
  (end-keys-list (array (array unsigned-char)))
  (end-keys-list-sizes (array size-t)))
