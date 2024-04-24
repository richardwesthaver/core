;;; rocksdb/comparator.lisp --- RocksDB Comparators

;;

;;; Code:
(in-package :rocksdb)

;; TODO 2023-12-11: 
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

(export '(rocksdb-comparator-destroy rocksdb-comparator-create rocksdb-comparator-with-ts-create))
