;;; rocksdb/slicetransform.lisp --- RocksDB SliceTransform

;; SliceTransformations (prefix extraction for bloom filters)

;;; Refs:

;; https://github.com/facebook/rocksdb/wiki/Prefix-Seek
;; https://github.com/facebook/rocksdb/blob/main/include/rocksdb/slice_transform.h

;;; Commentary:

#|
A SliceTransform is a generic pluggable way of transforming one string to
another. Its primary use-case is in configuring rocksdb to store prefix blooms
by setting prefix_extractor in ColumnFamilyOptions.
|#

;;; Code:
(in-package :rocksdb)

(defvar *rocksdb-transform-lambda-list*
  '((key (* unsigned-char))
    (len size-t)
    (dst-len (* size-t))))

(defvar *rocksdb-in-domain-lambda-list*
  '((state (* t))
    (key (* unsigned-char))
    (len size-t)))

(defvar *rocksdb-in-range-lambda-list*
  '((state (* t))
    (key (* unsigned-char))
    (len size-t)))

(define-alien-type rocksdb-transform-function
    (function (* unsigned-char)
        (* unsigned-char)
        size-t
      (* size-t)))

(define-alien-type rocksdb-in-domain-function
    (function boolean
        (* t) ;;state
        (array unsigned-char) ;;key
      size-t)) ;;len

(define-alien-type rocksdb-in-range-function
    (function unsigned-char
        (* t) ;;state
        (array unsigned-char) ;;key 
      size-t)) ;;len

(defar rocksdb-slicetransform-create (* rocksdb-slicetransform)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (transform (* rocksdb-transform-function))
  (in-domain (* rocksdb-in-domain-function))
  (in-range (* rocksdb-in-range-function))
  (name (* rocksdb-name-function)))

(defar rocksdb-slicetransform-create-noop (* rocksdb-slicetransform))

(defar rocksdb-slicetransform-create-fixed-prefix (* rocksdb-slicetransform)
  (n size-t))

(defar rocksdb-slicetransform-destroy void (st (* rocksdb-slicetransform)))

(locally
    (declare (sb-ext:muffle-conditions style-warning))
  (define-alien-callable rocksdb-transform-default (* unsigned-char)
      ((key (* unsigned-char))
       (isize size-t)
       (osize (* size-t)))
    key)

  (define-alien-callable rocksdb-in-domain-default boolean
      ((state (* t))
       (key (array unsigned-char))
       (len size-t))
    1)

  (define-alien-callable rocksdb-in-range-default boolean
      ((state (* unsigned))
       (key (* unsigned-char))
       (len size-t))
    1))
