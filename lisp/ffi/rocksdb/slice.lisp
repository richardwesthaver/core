;;; rocksdb/slice.lisp --- RocksDB SliceTransform and PinnableSlice

;; These are used primarily in transactions

;;; Refs:

;; https://rocksdb.org/blog/2017/08/24/pinnableslice.html

;; https://github.com/facebook/rocksdb/blob/main/include/rocksdb/slice_transform.h

;;; Code:
(in-package :rocksdb)
