;;; rocksdb/comparator.lisp --- RocksDB Comparators

;;

;;; Code:
(in-package :rocksdb)
;; TODO 2023-12-11: 
;; (define-alien-routine rocksdb-comparator-create (* rocksdb-comparator)
;;   (state (* void))
;;   (destructor (* void))
;;   (compare (* int))
;;   (name (* unsigned-char)))

(define-alien-routine rocksdb-comparator-destroy void (self (* rocksdb-comparator)))

;; (define-alien-routine rocksdb-comparator-with-ts-create (* rocksdb-comparator)
;;   (state (* void))
;;   (destructor (* void))
;;   (compare (* int))
;;   (compare-ts (* int))
;;   (compare-without-ts (* int))
;;   (name (* unsigned-char)))

(export '(rocksdb-comparator-destroy))
