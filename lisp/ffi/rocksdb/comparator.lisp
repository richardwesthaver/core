;;; rocksdb/comparator.lisp --- RocksDB Comparators

;; RocksDB Lisp Comparator API

;;; Commentary:

;; ref: https://github.com/facebook/rocksdb/blob/main/include/rocksdb/comparator.h
#|
// Three-way comparison.  Returns value:
//   < 0 iff "a" < "b",
//   == 0 iff "a" == "b",
//   > 0 iff "a" > "b"
// Note that Compare(a, b) also compares timestamp if timestamp size is
// non-zero. For the same user key with different timestamps, larger (newer)
// timestamp comes first.
|#
;;; Code:
(in-package :rocksdb)

(define-alien-type rocksdb-compare-function
  (function int
            (* t)
            c-string
            size-t
            c-string
            size-t))

(define-alien-type rocksdb-compare-ts-function
  (function int
            (* t)
            c-string
            size-t
            c-string
            size-t))

(define-alien-type rocksdb-compare-without-ts-function
  (function int
            (* t)
            c-string
            size-t
            unsigned-char
            c-string
            size-t
            unsigned-char))

(define-alien-routine rocksdb-comparator-create (* rocksdb-comparator)
  (state (* t))
  (destructor (* t))
  (compare (* int))
  (name (* unsigned-char)))

;; (rocksdb-comparator-create nil nil (make-alien int 1) (make-alien unsigned-char 10))

(define-alien-routine rocksdb-comparator-destroy void (self (* rocksdb-comparator)))

(define-alien-routine rocksdb-comparator-with-ts-create (* rocksdb-comparator)
  (state (* t))
  (destructor (* t))
  (compare (* int))
  (compare-ts (* int))
  (compare-without-ts (* int))
  (name (* unsigned-char)))
