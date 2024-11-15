;;; rocksdb/slicetransform.lisp --- RocksDB SliceTransform

;; SliceTransformations (prefix extraction for bloom filters)

;;; Refs:

;; https://github.com/facebook/rocksdb/blob/main/include/rocksdb/slice_transform.h

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

(define-alien-routine rocksdb-slicetransform-create (* rocksdb-slicetransform)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (transform (* rocksdb-transform-function))
  (in-domain (* rocksdb-in-domain-function))
  (in-range (* rocksdb-in-range-function))
  (name (* rocksdb-name-function)))

(define-alien-routine rocksdb-slicetransform-create-noop (* rocksdb-slicetransform))

(define-alien-routine rocksdb-slicetransform-create-fixed-prefix (* rocksdb-slicetransform)
  (n size-t))

(define-alien-routine rocksdb-slicetransform-destroy void (st (* rocksdb-slicetransform)))

(define-alien-callable rocksdb-transform-default (* unsigned-char)
    ((key (* unsigned-char))
     (isize size-t)
     (osize (* size-t)))
  (declare (ignore isize osize))
  key)

(define-alien-callable rocksdb-in-domain-default boolean
    ((state (* t))
     (key (array unsigned-char))
     (len size-t))
  (declare (ignore state key len))
  t)

(define-alien-callable rocksdb-in-range-default boolean
    ((state (* t))
     (key (array unsigned-char))
     (len size-t))
  (declare (ignore state key len))
  t)
