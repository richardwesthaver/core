;;; rocksdb/slicetransform.lisp --- RocksDB SliceTransform

;; These are used primarily in transactions

;;; Refs:

;; https://rocksdb.org/blog/2017/08/24/pinnableslice.html

;; https://github.com/facebook/rocksdb/blob/main/include/rocksdb/slice_transform.h

;;; Code:
(in-package :rocksdb)

(define-alien-type rocksdb-transform-function
    (function (array unsigned-char)
              (array unsigned-char)
              size-t
              (* size-t)))

(define-alien-type rocksdb-in-domain-function
  (function unsigned-char
            (* t)
            (array unsigned-char)
            size-t))

(define-alien-type rocksdb-in-range-function
  (function unsigned-char
            (* t)
            (array unsigned-char)
            size-t))

(define-alien-routine rocksdb-slicetransform-create (* rocksdb-slicetransform)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (transform (* rocksdb-transform-function))
  (in-domain (* rocksdb-in-domain-function))
  (in-range (* rocksdb-in-range-function))
  (name (* rocksdb-name-function)))

(define-alien-routine rocksdb-slicetransform-create-noop (* rocksdb-slicetransform))

(define-alien-routine rocksdb-slicetransform-create-fixed-prefix (* rocksdb-slicetransform)
  (n size-t))

(define-alien-routine rocksdb-slicetransform-destroy void (st (* rocksdb-slicetransform)))
