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

;;; Options
(defmacro with-latest-options (db-path (db-opts-var cf-names-var cf-opts-var) &body body)
  ;;  TODO 2024-09-26: ignore unknown?
  (with-gensyms (db-opts cf-names cf-opts)
    `(with-alien ((,db-opts (* rocksdb-options))
                  (,cf-names (* c-string))
                  (,cf-opts (* (* rocksdb-options)))
                  (ncols size-t)
                  (errptr rocksdb-errptr))
       (rocksdb-load-latest-options 
        ,db-path 
        (rocksdb-create-default-env) 
        t
        (rocksdb-cache-create-lru 1080)
        (addr ,db-opts)
        (addr ncols)
        (addr ,cf-names)
        (addr ,cf-opts)
        errptr)
       (let ((,db-opts-var ,db-opts)
             (,cf-names-var (coerce
                             (loop for i below ncols
                                   collect (deref ,cf-names i))
                             'vector))
             (,cf-opts-var (coerce
                            (loop for i below ncols
                                  collect (deref ,cf-opts i))
                            'vector)))
         (unwind-protect ,@body
           (rocksdb-load-latest-options-destroy ,db-opts ,cf-names ,cf-opts ncols))))))
      
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
         (define-alien-callable ,mname c-string () (string ',name))
         (defun ,mcreate ()
           (rocksdb-mergeoperator-create ,state
                                         (alien-sap (alien-callable-function ',destructor))
                                         (alien-sap (alien-callable-function ',fmerge))
                                         (alien-sap (alien-callable-function ',pmerge))
                                         (alien-sap (alien-callable-function ',delete))
                                         (alien-sap (alien-callable-function ',mname)))))))

;;; SliceTransforms
(defmacro define-slicetransform (name &body body))
(defmacro define-comparator (name &body body))
