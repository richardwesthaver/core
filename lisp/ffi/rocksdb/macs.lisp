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

(defmacro with-errptr (sym &body body)
  `(let ((,sym (alien-sap (make-alien (* (* t))))))
     (setf (deref (sap-alien ,sym (* (* t)))) nil)
     (unwind-protect (progn ,@body)
       (unless (null-alien (deref (sap-alien ,sym (* (* t)))))
         (rocksdb-c-error ,sym)))))

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
       #.*rocksdb-full-merge-lambda-list*
     ,@body))

(defmacro define-partial-merge-op (name &body body)
  `(define-alien-callable ,name (* t)
       #.*rocksdb-partial-merge-lambda-list*
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
(defmacro define-transform-function (name &body body)
  `(define-alien-callable ,name (* unsigned-char)
       ,*rocksdb-transform-lambda-list*
     ,@body))

(defmacro define-in-domain-function (name &body body)
  `(define-alien-callable ,name (* unsigned-char)
       ,*rocksdb-in-domain-lambda-list*
     ,@body))

(defmacro define-in-range-function (name &body body)
  `(define-alien-callable ,name (* unsigned-char)
       ,*rocksdb-in-range-lambda-list*
     ,@body))

(defmacro define-slicetransform (name &key (destructor 'rocksdb-destructor)
                                           state
                                           transform
                                           in-domain
                                           in-range)
  (with-gensyms (in-domain-fn in-range-fn transform-fn sname screate)
    (setf in-domain-fn (symbolicate name "-IN-DOMAIN")
          in-range-fn (symbolicate name "-IN-RANGE")
          sname (symbolicate name "-SLICETRANSFORM-NAME")
          transform-fn (symbolicate name "-TRANSFORM")
          screate (symbolicate "CREATE-" name "-TRANSFORM"))
    `(progn
       (define-transform-function ,transform-fn ,@transform)
       (define-in-domain-function ,in-domain-fn ,@in-domain)
       (define-in-range-function ,in-range-fn ,@in-range)
       (define-alien-callable ,sname c-string () (string ',name))
       (defun ,screate ()
         (rocksdb-slicetransform-create ,state
                                        (alien-sap (alien-callable-function ',destructor))
                                        (alien-sap (alien-callable-function ',transform))
                                        (alien-sap (alien-callable-function ',in-domain-fn))
                                        (alien-sap (alien-callable-function ',in-range-fn))
                                        (alien-sap (alien-callable-function ',sname)))))))
;;; Comparator
(defmacro define-compare-without-ts-function (name &body body)
  `(define-alien-callable ,name int
       ((state (* t))
        (a (* unsigned-char))
        (alen size-t)
        (atsp unsigned-char)
        (bts (* unsigned-char))
        (btslen size-t)
        (btsp unsigned-char))
     ,@body))

(defmacro define-compare-ts-function (name &body body)
  `(define-alien-callable ,name int
       ((state (* t))
        (ats (* unsigned-char))
        (atslen size-t)
        (bts (* unsigned-char))
        (btslen size-t))
     ,@body))
        
(defmacro define-compare-function (name &body body)
  `(define-alien-callable ,name int
       ((state (* t))
        (a (* unsigned-char))
        (alen size-t)
        (b (* unsigned-char))
        (blen size-t))
       ,@body))

(defmacro define-comparator (name &key compare (destructor 'rocksdb-destructor) state)
  "Define a RocksDB Comparator."
  (with-gensyms (cname cfn ccreate)
    (setf cname (symbolicate name "-COMPARATOR-NAME")
          cfn (symbolicate name "-COMPARE")
          ccreate (symbolicate "CREATE-" name "-COMPARATOR"))
    `(progn
       (define-alien-callable ,cname c-string () (string ',name))
       (define-compare-function ,cfn ,@compare)
       (defun ,ccreate ()
         (rocksdb-comparator-create ,state 
                                    (alien-sap (alien-callable-function ',destructor))
                                    (alien-sap (alien-callable-function ',cfn))
                                    (alien-sap (alien-callable-function ',cname)))))))

(defmacro define-comparator-with-ts (name &key state compare compare-ts compare-without-ts (destructor 'rocksdb-destructor))
  "Define a RocksDB Comparator which is timestamp-aware."
  (with-gensyms (cname-ts cfn cfn-ts cfn-without-ts ccreate-ts)
    (setf cname-ts (symbolicate name "-COMPARATOR-WITH-TS-NAME")
          cfn (symbolicate name "-COMPARE")
          cfn-ts (symbolicate name "-COMPARE-TS")
          cfn-without-ts (symbolicate name "-COMPARE-WITHOUT-TS")
          ccreate-ts (symbolicate "CREATE-" name "-COMPARATOR-WITH-TS"))
    `(progn
       (define-comparator ,name :compare ,compare :destructor ,destructor :state ,state)
       (define-alien-callable ,cname-ts c-string () (string ',(symbolicate name "-TS")))
       (define-compare-ts-function ,cfn-ts ,@compare-ts)
       (define-compare-without-ts-function ,cfn-without-ts ,@compare-without-ts)
       (defun ,ccreate-ts ()
         (rocksdb-comparator-with-ts-create ,state
                                            (alien-sap (alien-callable-function ',destructor))
                                            (alien-sap (alien-callable-function ',cfn))
                                            (alien-sap (alien-callable-function ',cfn-ts))
                                            (alien-sap (alien-callable-function ',cfn-without-ts))
                                            (alien-sap (alien-callable-function ',cname-ts)))))))

;;; Compaction Filter
(defmacro define-filter-function (name &body body)
  `(define-alien-callable ,name unsigned-char
      ((state (* t))
       (level int)
       (key (array unsigned-char))
       (key-length size-t)
       (existing-val (array unsigned-char))
       (existing-val-length size-t)
       (new-val (* (array unsigned-char)))
       (new-val-length (* size-t))
       (value-changed (* unsigned-char)))
    ,@body))

(defmacro define-create-filter-function (name destructor-fn filter-fn name-fn)
  `(define-alien-callable ,name (* rocksdb-compactionfilter)
       ((state (* t))
        (context (* rocksdb-compactionfiltercontext)))
     (rocksdb-compactionfilter-create state 
                                      (alien-sap (alien-callable-function ',destructor-fn))
                                      (alien-sap (alien-callable-function ',filter-fn))
                                      (alien-sap (alien-callable-function ',name-fn)))))
     
(defmacro define-compaction-filter (name &key (destructor 'rocksdb-destructor)
                                              filter)
  (with-gensyms (filter-fn cname ccreate)
    (setf filter-fn (symbolicate name "-FILTER")
          cname (symbolicate name "-COMPACTION-FILTER-NAME")
          ccreate (symbolicate "CREATE-" name "COMPACTION-FILTER"))
    `(progn
       (define-alien-callable ,cname c-string () (string ',name))
       (define-filter-function ,filter-fn ,@filter)
       (define-create-filter-function ,ccreate
           (alien-sap (alien-callable-function ',destructor))
         (alien-sap (alien-callable-function ',filter-fn))
         (alien-sap (alien-callable-function ',cname))))))
