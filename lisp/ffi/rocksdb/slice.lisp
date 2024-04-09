;;; rocksdb/slice.lisp --- RocksDB SliceTransform and PinnableSlice

;; These are used primarily in transactions

;;; Refs:

;; https://rocksdb.org/blog/2017/08/24/pinnableslice.html

;; https://github.com/facebook/rocksdb/blob/main/include/rocksdb/slice_transform.h

;;; Code:
(in-package :rocksdb)

(define-alien-routine rocksdb-slicetransform-create (* rocksdb-slicetransform)
  (state (* t))
  (destructor (* t))
  (transform (* t))
  (in-domain (* t))
  (in-range (* t))
  (name (* t)))

(define-alien-routine rocksdb-slicetransform-create-noop (* rocksdb-slicetransform))

(define-alien-routine rocksdb-slicetransform-create-fixed-prefix (* rocksdb-slicetransform)
  (n size-t))

(define-alien-routine rocksdb-slicetransform-destroy void (st (* rocksdb-slicetransform)))

(export '(rocksdb-slicetransform-create rocksdb-slicetransform-create-noop
          rocksdb-slicetransform-destroy rocksdb-slicetransform-create-fixed-prefix))
