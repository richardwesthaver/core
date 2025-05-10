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
            (* unsigned-char)
            size-t
            (* unsigned-char)
            size-t))

(define-alien-type rocksdb-compare-ts-function
  (function int
            (* t)
            (* unsigned-char)
            size-t
            (* unsigned-char)
            size-t))

(define-alien-type rocksdb-compare-without-ts-function
  (function int
            (* t)
            (* unsigned-char)
            size-t
            unsigned-char
            (* unsigned-char)
            size-t
            unsigned-char))

(defar rocksdb-comparator-create (* rocksdb-comparator)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (compare (* rocksdb-compare-function))
  (name (* rocksdb-name-function)))

(defar rocksdb-comparator-destroy void (self (* rocksdb-comparator)))

(defar rocksdb-comparator-with-ts-create (* rocksdb-comparator)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (compare (* rocksdb-compare-function))
  (compare-ts (* rocksdb-compare-ts-function))
  (compare-without-ts (* rocksdb-compare-without-ts-function))
  (name (* rocksdb-name-function)))

(define-alien-callable rocksdb-compare-never-name c-string () (make-alien-string "compare-never"))

(define-alien-callable rocksdb-compare-never int
    ((state (* t))
     (a (* unsigned-char))
     (alen size-t)
     (b (* unsigned-char))
     (blen size-t))
  (declare (ignore state a alen b blen))
  0)

(define-alien-callable rocksdb-compare-never-with-ts int
    ((state (* t))
     (a (* unsigned-char))
     (alen size-t)
     (b (* unsigned-char))
     (blen size-t))
  (declare (ignore state a alen b blen))
  0)

(define-alien-callable rocksdb-compare-never-without-ts int
    ((state (* t))
     (a (* unsigned-char))
     (alen size-t)
     (a-ts unsigned-char)
     (b (* unsigned-char))
     (blen size-t)
     (b-ts unsigned-char))
  (declare (ignore state a alen a-ts b blen b-ts))
  0)
