;;; store.lisp --- Data Store Protocols

;; Support for Lisp Stores.

;;; Commentary:

;; Inspired by Elephant

;; STOREs differ from DBs in that they always prefer transactions over simple
;; set/get.

;;; Code:
(defpackage :obj/store
  (:nicknames :store)
  (:use :cl :std :stored)
  (:export
   #:register-instance
   #:cache-instance
   #:get-cached-instance
   #:uncache-instance
   #:flush-instance-cache))

(in-package :obj/store)

(defvar *store* nil)

(defgeneric register-instance (self class intance))
(defgeneric cache-instance (self obj))
(defgeneric get-cached-instance (self oid))
(defgeneric uncache-instance (self oid))
(defgeneric flush-instance-cache (self))

(defun make-cache-table (&rest args)
  "Make a value-weak hashtable. When value gets collected so does the key."
  (apply 'make-hash-table :weakness :value args))

(defclass store () 
  ((spec :type list
         :accessor spec
         :initarg :spec
         :documentation "Data store initialization functions are
         expected to initialize :spec on the call to
         make-instance")
   (schema-table :reader controller-schema-table
                 :documentation "Schema id to schema database table")
   (schema-name-index :reader controller-schema-name-index
                      :documentation "Schema name to schema database table")
   (schema-cache :accessor controller-schema-cache :initform (make-cache-table :test 'eq)
                 :documentation "This is a cache of class schemas stored in the database indexed by classid")
   (schema-classes :accessor controller-schema-classes :initform nil
                      :documentation "Maintains a list of all classes that have a cached schema value so we can shutdown cleanly")
   (schema-cache-lock :accessor controller-schema-cache-lock :initform (sb-concurrency:make-frlock :name "cache-lock")
                        :documentation "Protection for updates to the cache from multiple threads.  
                                        Do not override.")
   ;; Instance storage
   (instance-table :reader controller-instance-table
                  :documentation "Contains btree of oid to class ids")
   (instance-class-index :reader controller-instance-class-index
                         :documentation "A reverse map of class id to oid")
   (instance-cache :accessor controller-instance-cache :initform (make-cache-table :test 'eql)
                   :documentation 
                   "This is an instance cache and part of the
                    metaclass protocol.  Data stores should not
                    override the default behavior.")
   (instance-cache-lock :accessor controller-instance-cache-lock :initform (make-mutex :name "instance-cache")
                        :documentation "Protection for updates to
                        the cache from multiple threads.  Do not
                        override.")
   ;; Root table for all indices
   (index-table :reader controller-index-table
               :documentation 
               "This is another root for class indexing that is
               also a data store specific persistent btree instance
               with a unique OID that persists between sessions.
               No cache is needed because we cache in the class slots.")
   (serializer :accessor serializer :initform nil)
   (deserializer :accessor deserializer :initform nil)))

(defmethod print-object ((self store) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (second (spec self)))))

(defmethod initialize-instance :before  ((instance stored)
                                         &rest initargs
                                         &key oid
                                              store)
  "Each persistent instance has an oid and a home controller spec"
  (declare (ignore initargs))
  (initial-stored-setup instance :oid oid :store store))

(defun initial-stored-setup (instance &key oid store)
  (assert store)
  (if oid
      (setf (oid instance) oid)
      (register-new-instance instance (class-of instance) store))
  (setf (spec instance) (spec store))
  (cache-instance store instance))

(defun register-new-instance (instance class store)
  (setf (oid instance) (next-oid store))
  (register-instance store class instance))

(defun check-valid-store (store)
  (if-let ((ok (subtypep (type-of store) 'store)))
    ok
    (error "This function requires a valid store controller")))
