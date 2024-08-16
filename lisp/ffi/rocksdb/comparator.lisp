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

(define-alien-type rocksdb-compare-with-ts-function
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
  (destructor (* rocksdb-destructor-function))
  (compare (* rocksdb-compare-function))
  (name (* rocksdb-name-function)))

;; (rocksdb-comparator-create nil nil (make-alien int 1) (make-alien unsigned-char 10))

(define-alien-routine rocksdb-comparator-destroy void (self (* rocksdb-comparator)))

(define-alien-routine rocksdb-comparator-with-ts-create (* rocksdb-comparator)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (compare (* rocksdb-compare-function))
  (compare-with-ts (* rocksdb-compare-with-ts-function))
  (compare-without-ts (* rocksdb-compare-without-ts-function))
  (name (* rocksdb-name-function)))

(define-alien-callable rocksdb-compare-never-name c-string () (make-alien-string "compare-never"))

(define-alien-callable rocksdb-compare-never int
    ((state (* t))
     (a c-string)
     (alen size-t)
     (b c-string)
     (blen size-t))
  (declare (ignore state a alen b blen))
  0)

(define-alien-callable rocksdb-compare-never-with-ts int
    ((state (* t))
     (a c-string)
     (alen size-t)
     (b c-string)
     (blen size-t))
  (declare (ignore state a alen b blen))
  0)

(define-alien-callable rocksdb-compare-never-without-ts int
    ((state (* t))
     (a c-string)
     (alen size-t)
     (a-ts unsigned-char)
     (b c-string)
     (blen size-t)
     (b-ts unsigned-char))
  (declare (ignore state a alen a-ts b blen b-ts))
  0)

    
