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

(eval-always
  (defvar *rocksdb-partial-merge-lambda-list*
    '((key (array unsigned-char))
      (klen size-t)
      (ops (array (array unsigned-char)))
      (ops-length (* size-t))
      (num-ops size-t)
      (success (array unsigned-char))
      (new-vlen (* size-t))))

  (defvar *rocksdb-full-merge-lambda-list*
    '((key (array unsigned-char))
      (klen size-t)
      (existing-val (array unsigned-char))
      (existing-vlen size-t)
      (ops (array (array unsigned-char)))
      (ops-length (* size-t))
      (num-ops size-t)
      (success (array unsigned-char))
      (new-vlen (* size-t)))))

#|
Gives the client a way to express the read -> modify -> write semantics
key:         (IN) The key that's associated with this merge operation.
existing:    (IN) null indicates that the key does not exist before this op
operand_list:(IN) the sequence of merge operations to apply, front() first.
new_value:  (OUT) Client is responsible for filling the merge result here
logger:      (IN) Client could use this to log errors during merge.

Return true on success. Return false failure / error / corruption.
|#
;; FullMerge() is used when a Put/Delete is the *existing_value (or null)
(define-alien-type rocksdb-full-merge-function
    (function boolean
              (array unsigned-char)
              size-t
              (array (array unsigned-char))
              (array size-t)
              int
              (array unsigned-char)
              (* size-t)))

#|
This function performs merge(left_op, right_op)
when both the operands are themselves merge operation types.
Save the result in *new_value and return true. If it is impossible
or infeasible to combine the two operations, return false instead.
|#
;; PartialMerge() is used to combine two-merge operands (if possible)
(define-alien-type rocksdb-partial-merge-function
    (function boolean
              (array unsigned-char)
              size-t
              (array (array unsigned-char))
              (array size-t)
              int
              (array unsigned-char)
              (* size-t)))

(define-alien-type rocksdb-delete-value-function
  (function void
            (array unsigned-char)
            size-t))

(define-alien-type rocksdb-destructor-function
  (function void (* t)))

#|
The name of the MergeOperator. Used to check for MergeOperator
mismatches (i.e., a DB created with one MergeOperator is
accessed using a different MergeOperator)
|#
(define-alien-type rocksdb-name-function
    (function c-string))

;; (sb-alien::define-alien-callable mangle int () 0)

(define-alien-routine rocksdb-mergeoperator-create (* rocksdb-mergeoperator)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (full-merge (* rocksdb-full-merge-function))
  (partial-merge (* rocksdb-partial-merge-function))
  (delete-value (* rocksdb-delete-value-function))
  (name (* rocksdb-name-function)))

#| [[file:~/dev/comp/core/c/rocksdb.h::/* Merge Operator */]] |#

(define-alien-routine rocksdb-mergeoperator-destroy void (self (* rocksdb-mergeoperator)))

(define-alien-callable rocksdb-destructor void ((self (* t)))
  (free-alien self)
  (values))

(define-alien-callable rocksdb-name c-string () (make-alien-string (symbol-name (gensym "rocksdb:"))))

;;; Associative Merge Ops
;;;; Concat Merge
(define-alien-callable rocksdb-concat-merge-name c-string () (make-alien-string "cc:concat"))

(define-alien-callable rocksdb-concat-full-merge boolean #.*rocksdb-full-merge-lambda-list*
  (log:trace!
   "Applying CC:CONCAT full merge..."
   (list key klen existing-val existing-vlen ops ops-length num-ops success new-vlen))
  (let* ((oplens (loop for i below num-ops
                       collect (deref ops-length i)))
         (opslen (reduce '+ oplens))
         (evlen existing-vlen))
      (with-alien ((len size-t (+ evlen opslen))
                   (val (array unsigned-char)
                        (cast (make-alien unsigned-char len) (array unsigned-char))))
        (loop for i below evlen
              do (setf (deref val i) (deref existing-val i)))
        (loop with shift = 0
              for i below num-ops
              for j in oplens
              with x = (deref ops i)
              do (loop for l below j
                       do (setf (deref val (+ shift l evlen)) (deref ops i l)))
              do (incf shift j))
        (setf new-vlen (addr len)
              success val)
        1)))

(define-alien-callable rocksdb-concat-partial-merge boolean #.*rocksdb-partial-merge-lambda-list*
  (log:trace! 
   "Applying CC:CONCAT partial merge..."
   (list key klen ops ops-length num-ops success new-vlen))
  1)

(define-alien-callable rocksdb-delete-value void
    ((state (* t))
     (value c-string)
     (value-length size-t))
  (declare (ignore state))
  ;; TODO 2024-08-18: test if this is needed
  (unless (zerop value-length)
    (log:trace! "deleting value:" value)
    (setf value nil))
  (values))

;;;; Index Merge
(define-alien-callable rocksdb-index-merge-name c-string () (make-alien-string "cc:index"))

(define-alien-callable rocksdb-index-partial-merge boolean #.*rocksdb-partial-merge-lambda-list*)
    
(define-alien-callable rocksdb-index-full-merge boolean #.*rocksdb-full-merge-lambda-list*)
    
 
