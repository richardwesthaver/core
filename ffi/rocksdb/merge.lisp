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
    '((state (* t))
      (key (* unsigned-char))
      (klen size-t)
      (ops (* (* unsigned-char)))
      (ops-length (* size-t))
      (num-ops size-t)
      (success (* unsigned-char))
      (new-vlen (* size-t))))

  (defvar *rocksdb-full-merge-lambda-list*
    '((state (* t))
      (key (* unsigned-char))
      (klen size-t)
      (existing-val (* unsigned-char))
      (existing-vlen size-t)
      (ops (* (* unsigned-char)))
      (ops-length (* size-t))
      (num-ops int)
      (success (* unsigned-char))
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
    (function (* char)
              (* t)
              (* unsigned-char)
              size-t
              (* unsigned-char)
              size-t
              (* (* unsigned-char))
              (* size-t)
              int
              (* unsigned-char)
              (* size-t)))

#|
This function performs merge(left_op, right_op)
when both the operands are themselves merge operation types.
Save the result in *new_value and return true. If it is impossible
or infeasible to combine the two operations, return false instead.
|#
;; PartialMerge() is used to combine two-merge operands (if possible)
(define-alien-type rocksdb-partial-merge-function
    (function (* char)
              (* t)
              (* unsigned-char)
              size-t
              (* (* unsigned-char))
              (* size-t)
              int
              (* unsigned-char)
              (* size-t)))

(define-alien-type rocksdb-delete-value-function
  (function void
            (* unsigned-char)
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

(defar rocksdb-mergeoperator-create (* rocksdb-mergeoperator)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (full-merge (* rocksdb-full-merge-function))
  (partial-merge (* rocksdb-partial-merge-function))
  (delete-value (* rocksdb-delete-value-function))
  (name (* rocksdb-name-function)))

#| [[file:~/dev/comp/core/c/rocksdb.h::/* Merge Operator */]] |#

(defar rocksdb-mergeoperator-destroy void (self (* rocksdb-mergeoperator)))

(define-alien-callable rocksdb-destructor void ((self (* t)))
  (free-alien self)
  (values))

(define-alien-callable rocksdb-name c-string () (make-alien-string #.(symbol-name (gensym "rocksdb:"))))

;;; Associative Merge Ops
;;;; Concat Merge
(define-alien-callable rocksdb-concat-merge-name c-string () (make-alien-string "cc:concat"))

(define-alien-callable rocksdb-concat-full-merge (* char) #.*rocksdb-full-merge-lambda-list*
  (declare (ignore state))
  (log:trace!
   (format nil "Applying CC:CONCAT full merge with ~A operands" num-ops))
  (log:trace! :key key :klen klen)
  (let ((len existing-vlen)
        (opslen (alien-sap ops-length))
        (ret (make-alien char)))
    (unless (null-alien existing-val)
        (loop for i below existing-vlen
            do (setf (deref ret i) (deref existing-val i))))
    (unless (zerop num-ops)
      (loop for i below num-ops
            with slen = #.(alien-size (* size-t) :bytes)
            with s = #.(alien-size (* (* unsigned-char)) :bytes)
            with olen = (deref (sap-alien (sb-alien::sap+ opslen (* slen i)) (* size-t)))
            do (loop for l below olen
                     do (setf (deref ret (+ len l)) (deref (deref ops i) l)))
            do (incf len olen)))
    (setf (deref new-vlen) len
          (deref success) 1)
    ret))

(define-alien-callable rocksdb-concat-partial-merge (* char) #.*rocksdb-partial-merge-lambda-list*
  (declare (ignore state))
  (log:trace! 
   "Applying CC:CONCAT partial merge..."
   (list key klen ops ops-length num-ops success new-vlen))
    (setf (deref success) 0)
  nil)

(define-alien-callable rocksdb-delete-value void
    ((state (* t))
     (value (* unsigned-char))
     (value-length size-t))
  (declare (ignore state value-length))
  (unless (null-alien value)
    (setf value nil))
  (values))

;;;; Index Merge
(define-alien-callable rocksdb-index-merge-name c-string () (make-alien-string "cc:index"))

(define-alien-callable rocksdb-index-full-merge (* unsigned-char) #.*rocksdb-full-merge-lambda-list*
  (declare (ignore state))
  (log:trace! "Applying CC:INDEX full merge with ~A operands" num-ops)
  (log:trace! :key key :klen klen)
  (let ((len (if (zerop existing-vlen) 1 existing-vlen))
        (opslen (alien-sap ops-length))
        (ret 0))
    (unless (null-alien existing-val)
      (incf ret
            (std:octets-to-integer
             (coerce
              (loop for i below existing-vlen
                    collect (deref existing-val i))
              'std:octet-vector))))
    (unless (zerop num-ops)
      (loop for i below num-ops
            with slen = #.(alien-size (* size-t) :bytes)
            with s = #.(alien-size (* (* unsigned-char)) :bytes)
            with olen = (deref (sap-alien (sb-alien::sap+ opslen (* slen i)) (* size-t)))
            do (incf ret
                     (std:octets-to-integer
                      (coerce
                       (loop for l below olen
                             collect (deref (deref ops i) l))
                       'std:octet-vector)))))
    (setf (deref new-vlen) len
          (deref success) 1)
    (octets-to-alien (std:integer-to-octets ret (* 8 len)))))

(define-alien-callable rocksdb-index-partial-merge boolean #.*rocksdb-partial-merge-lambda-list*
  (declare (ignore state key klen ops ops-length num-ops new-vlen))
  (setf (deref success) 0)
  0)
