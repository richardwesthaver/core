;;; rocksdb/merge.lisp --- RocksDB Merge Operators

;;

;;; Code:
(in-package :rocksdb)

(define-alien-type rocksdb-full-merge-function
    (function (* t)
              (array unsigned-char)
              size-t
              (array (array unsigned-char))
              (array size-t)
              int
              (array unsigned-char)
              (* size-t)))

(define-alien-type rocksdb-partial-merge-function
    (function (* t)
              (array unsigned-char)
              size-t
              (array (array unsigned-char))
              (array size-t)
              int
              (array unsigned-char)
              (* size-t)))

(define-alien-type rocksdb-delete-value-function
  (function (* t)
            (array unsigned-char)
            size-t))

(define-alien-type rocksdb-destructor-function
    (function (* t)))

(define-alien-type rocksdb-name-function
    (function c-string))

(deftype rocksdb-merge-operands () '(array (octet-vector)))

;; (sb-alien::define-alien-callable mangle int () 0)

;; (sb-alien::alien-callback
(define-alien-routine rocksdb-mergeoperator-create (* rocksdb-mergeoperator)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (full-merge (* rocksdb-full-merge-function))
  (partial-merge (* rocksdb-partial-merge-function))
  (delete-value (* rocksdb-delete-value-function))
  (name c-string))

#| [[file:~/dev/comp/core/c/rocksdb.h::/* Merge Operator */]] |#

(define-alien-routine rocksdb-mergeoperator-destroy void (self (* rocksdb-mergeoperator)))

(export '(rocksdb-mergeoperator-create rocksdb-mergeoperator-destroy
          rocksdb-full-merge-function rocksdb-partial-merge-function
          rocksdb-delete-value-function rocksdb-destructor-function))

;; TODO 2023-12-11: 
(deftype rocksdb-mergeoperator-function ()
  '(function (octet-vector (or octet-vector null) &rest t) (or null octet-vector)))

(define-alien-callable rocksdb-concat-full-merge (* t)
    ((key (array unsigned-char)) (klen size-t)
     (existing-val (array unsigned-char)) (existing-vlen size-t)
     (ops (array (array unsigned-char))) (ops-length (* size-t)) (num-ops size-t)
     (success (array unsigned-char))
     (new-vlen (* size-t)))
  nil)

(define-alien-callable rocksdb-concat-partial-merge (* t)
    ((key (array unsigned-char)) (klen size-t)
     (ops (array (array unsigned-char))) (ops-length (* size-t)) (num-ops size-t)
     (success (array unsigned-char))
     (new-vlen (* size-t)))
  nil)
