;;; rocksdb/merge.lisp --- RocksDB Merge Operators

;; RocksDB Lisp Merge Operator API

;;; Commentary:

;; When to use built-in ROCKSDB-MERGE:

;; - You have data that needs to be incrementally updated.

;; - You would usually need to read the data before knowing what the new value would be.

;; Oterwise as far as the FFI is concerned - which doesn't support
;; AssociateMerge, you should use the Generic Merge API.

;; When to use Associative Merge (unavailable in C/LISP API):

;; - merge operands are formatted the same as Put values AND

;; - it is okay to combine multiple operands into one

;; When to use Generic Merge (this API):

;; - you are unable to use Associate Merge

;; - it is possible to combine multiple operands

;;; Refs:

;; impl: https://github.com/facebook/rocksdb/wiki/Merge-Operator-Implementation

;; wiki: https://github.com/facebook/rocksdb/wiki/merge-operator

;;; Code:
(in-package :rocksdb)

;; FullMerge() is used when a Put/Delete is the *existing_value (or null)
(define-alien-type rocksdb-full-merge-function
    (function (* t)
              (array unsigned-char)
              size-t
              (array (array unsigned-char))
              (array size-t)
              int
              (array unsigned-char)
              (* size-t)))
;; PartialMerge() is used to combine two-merge operands (if possible)
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
  (function void (* t)))

(define-alien-type rocksdb-name-function
    (function c-string))

(deftype rocksdb-merge-operands () '(array (octet-vector)))

;; (sb-alien::define-alien-callable mangle int () 0)

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

(define-alien-callable rocksdb-delete-value (* t)
    ((val (array unsigned-char))
     (vlen size-t))
  (declare (ignore val vlen))
  nil)
 
(define-alien-callable rocksdb-destructor void ((self (* t)))
  (free-alien self))

(define-alien-callable rocksdb-name c-string () (make-alien-string (symbol-name (gensym "rocksdb:"))))

(define-alien-callable rocksdb-concat-full-merge (* t)
    ((key (array unsigned-char))
     (klen size-t)
     (existing-val (array unsigned-char))
     (existing-vlen size-t)
     (ops (array (array unsigned-char)))
     (ops-length (* size-t))
     (num-ops size-t)
     (success (array unsigned-char))
     (new-vlen (* size-t)))
  (log:debug! (list key klen existing-val existing-vlen ops ops-length num-ops success new-vlen))
  nil)

(define-alien-callable rocksdb-concat-partial-merge (* t)
    ((key (array unsigned-char))
     (klen size-t)
     (ops (array (array unsigned-char)))
     (ops-length (* size-t))
     (num-ops size-t)
     (success (array unsigned-char))
     (new-vlen (* size-t)))
  (log:debug! (list key klen ops ops-length num-ops success new-vlen))
  nil)
