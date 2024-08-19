;;; macs.lisp --- RocksDB Alien Macros

;; Convenience Macros for working with RocksDB Alien types

;;; Code:
(in-package :rocksdb)

(deftype rocksdb-mergeoperator-function ()
  '(function (octet-vector (or octet-vector null) &rest t) (or null octet-vector)))

(deftype rocksdb-comparator-function ()
  '(function (octet-vector octet-vector) (integer -1 1)))

(deftype rocksdb-compactionfilter-function ()
  ;;           level              key           val           new         changed
  '(function ((unsigned-byte 32) octet-vector octet-vector octet-vector) boolean))

(deftype rocksdb-logger-function ()
  '(function (unsigned-byte string) (values)))

;;; Merge Ops
(defmacro define-full-merge-op (name &body body)
  `(define-alien-callable ,name (* t)
       ,*rocksdb-full-merge-lambda-list*
     ,@body))

(defmacro define-partial-merge-op (name &body body)
  `(define-alien-callable ,name (* t)
       ,*rocksdb-partial-merge-lambda-list*
     ,@body))

(defmacro define-merge-operator (name state &key full
                                                 partial
                                                 (destructor 'rocksdb-destructor)
                                                 (delete 'rocksdb-delete-value))
  (with-gensyms (fmerge pmerge mcreate mname)
    (setf fmerge (symbolicate name "-FULL-MERGE")
          pmerge (symbolicate name "-PARTIAL-MERGE")
          mcreate (symbolicate "CREATE-" name "-MERGEOPERATOR")
          mname (symbolicate name "-MERGEOPERATOR-NAME"))
      `(progn
         (define-full-merge-op ,fmerge ,@full)
         (define-partial-merge-op ,pmerge ,@partial)
         (define-alien-callable ,mname c-string () ,name)
         (defun ,mcreate ()
           (rocksdb-mergeoperator-create ,state
                                         (alien-sap (alien-callable-function ',destructor))
                                         (alien-sap (alien-callable-function ',fmerge))
                                         (alien-sap (alien-callable-function ',pmerge))
                                         (alien-sap (alien-callable-function ',delete))
                                         (alien-sap (alien-callable-function ',mname)))))))
