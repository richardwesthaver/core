;;; obj/meta/pkg.lisp --- Meta-objects

;;

;;; Commentary:


;;;; Notes:

;; ordered? https://www.reddit.com/r/lisp/comments/n88x59/metaclasses_using_structures_or_speeding_up_slot/

;;;; Ref:

;; https://franz.com/support/documentation/11.0/mop/concepts.html

;;; Code:
(defpackage :obj/meta
  (:nicknames :meta)
  (:use :cl :std)
  (:export
   :class-equalp
   :*standard-metaobjects*
   :find-slot-def-by-name
   :find-direct-slot-def-by-name
   :find-slot-defs-by-type
   :find-slot-def-names-by-type
   :struct-slots-and-values
   :slots-and-values
   :struct-constructor))

(defpackage :obj/meta/stealth
  (:nicknames :meta/stealth :stealth)
  (:use :cl :std :obj/meta :sb-mop)
  (:export
   #:add-mixin
   #:define-stealth-mixin))

(defpackage :obj/meta/filtered
  (:nicknames :meta/filtered :filtered)
  (:use :cl :std :obj/meta :sb-mop)
  (:export
   :define-filtered-function :filtered :filtered-function :filtered-method
   :generic-function-filter-expression :generic-function-filters :method-filter :simple-filtered-function))

(defpackage :obj/meta/sealed
  (:nicknames :meta/sealed :sealed)
  (:use :cl :std :obj/meta)
  (:import-from :sb-pcl :eql-specializer :intern-eql-specializer
   :eql-specializer-object :funcallable-standard-class)
  (:import-from :sb-mop :class-finalized-p :finalize-inheritance
   :class-precedence-list :class-direct-superclasses :specializer :method-specializers
   :generic-function-argument-precedence-order :generic-function-name :generic-function-methods :class-direct-subclasses
   :class-prototype)
  (:export
   :ensure-specializer
   :specializer-type
   :specializer-prototype
   :specializer-direct-superspecializers
   :specializer-intersectionp
   :specializer-subsetp
   :domain
   :ensure-domain
   :method-domain
   :domain-specializers
   :domain-arity
   :domain-equal
   :domain-intersectionp
   :domain-subsetp

   :metaobject-sealable-p
   :class-sealable-p
   :generic-function-sealable-p
   :method-sealable-p
   :specializer-sealable-p

   :metaobject-sealed-p
   :class-sealed-p
   :generic-function-sealed-p
   :method-sealed-p
   :specializer-sealed-p

   :seal-class
   :seal-generic-function
   :seal-method
   :seal-domain
   :seal-specializer

   :method-properties
   :validate-method-property

   :static-call-signature
   :static-call-signature-types
   :static-call-signature-prototypes

   :sealed-domains
   :compute-static-call-signatures
   :externalizable-object-p
   :sealable-class
   :sealable-generic-function
   :sealable-standard-generic-function
   :potentially-sealable-method
   :potentially-sealable-standard-method))

(defpackage :obj/meta/fast
  (:nicknames :meta/fast :fast)
  (:use :cl :std :obj/meta/sealed :obj/meta)
  (:import-from :sb-int :gensymify)
  (:import-from :sb-walker :macroexpand-all)
  (:export :fast-generic-function :fast-method :inlineable :.lambda.))

(defpackage :obj/meta/lazy
  (:nicknames :meta/lazy :lazy)
  (:use :cl :std :obj/meta))

(defpackage :obj/meta/overloaded
  (:nicknames :meta/overloaded :overloaded)
  (:use :cl :std :obj/meta))

(defpackage :obj/meta/stored
  (:nicknames :meta/stored :stored)
  (:use :cl :std :obj/meta :sb-mop)
  (:export
   :stored-class :initialize-stored-class
   :stored-slot
   :stored
   :stored-object
   :stored-collection
   :oid
   :spec
   :stored-p
   :indexed-slot-names
   :indexed-slot-defs
   :stored-slot-definition
   :indexed-slot-definition
   :derived-slot-triggers
   :derived-fn
   :get-slot-def-index
   :add-slot-def-index
   :clear-slot-def-index
   :indexed-slot-base
   :indexed-slot-indices
   :get-store-schemas
   :get-class-indexing
   :get-cache-style
   :has-class-schema-p
   :find-slot-defs-by-type
   :migrate-class-index-p
   :class-indexing-enabled-p
   :defsclass
   :get-class-schema
   :drop-instance
   :register-instance
   :cache-instance
   :get-cached-instance
   :uncache-instance
   :flush-instance-cache
   :stored-slot-makunbound
   :stored-slot-boundp
   :stored-slot-writer
   :stored-slot-reader
   :get-store
   :read-oid
   :write-oid
   :stored-slot-names
   :all-stored-slot-names
   :all-single-valued-slot-defs
   :cached-slot-definition
   :cached-direct-slot-definition
   :transient-slot-definition
   :cached-slot-names
   :transient-p
   :transient-slot-names
   :database-allocation-p
   :slot-definition-allocation))

(defpackage :obj/meta/typed
  (:nicknames :meta/typed :typed)
  (:use :cl :std :obj/meta :sb-mop :stored)
  (:export
   #:type-num
   #:type<=
   #:type<
   #:type=
   #:array-type=
   #:array-type-from-byte
   #:byte-from-array-type
   #:int-byte-spec))

(defpackage :obj/meta/dynamic
  (:nicknames :meta/dynamic :dynamic)
  (:use :cl :std :obj/meta :std/macs)
  (:export :dset :dref :dynamic-class
   :slot-dlet :slot-dvar :slot-dvar*))

(in-package :obj/meta)

(defun class-equalp (c1 c2)
  (when (symbolp c1) (setf c1 (find-class c1)))
  (when (symbolp c2) (setf c2 (find-class c2)))
  (eq c1 c2))

(defun type-specifier-and (&rest type-specifiers)
  (let ((relevant (remove t type-specifiers)))
    (cond ((null relevant) t)
          ((null (cdr relevant)) (first relevant))
          (t `(and ,@relevant)))))

(defun type-specifier-or (&rest type-specifiers)
  (let ((relevant (remove nil type-specifiers)))
    (cond ((null relevant) nil)
          ((null (cdr relevant)) (first relevant))
          (t `(or ,@relevant)))))

(defun type-specifier-not (type-specifier)
  (cond ((eql type-specifier t) nil)
        ((eql type-specifier nil) t)
        (t `(not ,type-specifier))))

(defparameter *standard-metaobjects*
  (list (find-class 'standard-object)
        (find-class 'standard-class)
        (find-class 'standard-generic-function)
        (find-class 'standard-method)
        (find-class 'built-in-class)))

(defgeneric struct-constructor (class)
  (:documentation "Called to get the constructor name for a struct class. Users
                  should overload this when they want to serialize
                  non-standard constructor names. The default constructor
                  make-xxx will work by default. The argument is an eql style
                  type: i.e. of type (eql 'my-struct)"))

(defmethod struct-constructor ((class t))
  (symbol-function (intern (concatenate 'string "MAKE-" (symbol-name class))
                           (symbol-package class))))

;;; From ARNESI - Messing with the MOP

;; https://bese.common-lisp.dev/docs/arnesi/html/Messing_0020with_0020the_0020MOP.html#wrapping-standard_0020method_0020combination

(define-method-combination wrapping-standard
    (&key (around-order :most-specific-first)
          (before-order :most-specific-first)
          (primary-order :most-specific-first)
          (after-order :most-specific-last)
          (wrapping-order :most-specific-last)
          (wrap-around-order :most-specific-last))
  ((wrap-around (:wrap-around))
   (around (:around))
   (before (:before))
   (wrapping (:wrapping))
   (primary () :required t)
   (after (:after)))
  "Same semantics as standard method combination but allows
\"wrapping\" methods. Ordering of methods:

 (wrap-around
   (around
     (before)
     (wrapping
       (primary))
     (after)))

:warp-around, :around, :wrapping and :primary methods call the
next least/most specific method via call-next-method (as in
standard method combination).

The various WHATEVER-order keyword arguments set the order in
which the methods are called and be set to either
:most-specific-last or :most-specific-first."
  (labels ((effective-order (methods order)
             (ecase order
               (:most-specific-first methods)
               (:most-specific-last (reverse methods))))
           (call-methods (methods)
             (mapcar (lambda (meth) `(call-method ,meth))
                     methods)))
    (let* (;; reorder the methods based on the -order arguments
           (wrap-around (effective-order wrap-around wrap-around-order))
           (around (effective-order around around-order))
           (wrapping (effective-order wrapping wrapping-order))
           (before (effective-order before before-order))
           (primary (effective-order primary primary-order))
           (after (effective-order after after-order))
           ;; inital value of the effective call is a call its primary
           ;; method(s)
           (form (case (length primary)
                   (1 `(call-method ,(first primary)))
                   (t `(call-method ,(first primary) ,(rest primary))))))
      (when wrapping
        ;; wrap form in call to the wrapping methods
        (setf form `(call-method ,(first wrapping)
                                 (,@(rest wrapping) (make-method ,form)))))
      (when before
        ;; wrap FORM in calls to its before methods
        (setf form `(progn
                      ,@(call-methods before)
                      ,form)))
      (when after
        ;; wrap FORM in calls to its after methods
        (setf form `(multiple-value-prog1
                        ,form
                      ,@(call-methods after))))
      (when around
        ;; wrap FORM in calls to its around methods
        (setf form `(call-method ,(first around)
                                 (,@(rest around)
                                    (make-method ,form)))))
      (when wrap-around
        (setf form `(call-method ,(first wrap-around)
                                 (,@(rest wrap-around)
                                    (make-method ,form)))))
      form)))

(defun find-class-for-direct-slot (class def)
  (let ((list (sb-mop:compute-class-precedence-list class)))
    (labels ((rec (super)
               (if (null super)
                   nil
                   (aif (find-direct-slot-def-by-name super (sb-mop:slot-definition-name def))
                        (class-name super)
                        (rec (pop list))))))
      (rec class))))

;;; Slot Helpers
(defun find-direct-slot-def-by-name (class slot-name)
  (loop for slot-def in (sb-mop:class-direct-slots class)
        when (eq (sb-mop:slot-definition-name slot-def) slot-name)
        do (return slot-def)))

(defun find-slot-def-by-name (class slot-name)
  (loop for slot-def in (sb-mop:class-slots class)
        when (eq (sb-mop:slot-definition-name slot-def) slot-name)
        do (return slot-def)))

(defgeneric find-slot-defs-by-type (class type &optional by-subtype))
(defgeneric find-slot-def-names-by-type (class type &optional by-subtype))

(defun slots-and-values (o)
  "List of slot names followed by values for object"
  (loop for sd in (sb-mop:compute-slots (class-of o))
        for slot-name = (sb-mop:slot-definition-name sd)
        with ret = ()
        do
        (when (and (slot-boundp o slot-name)
                   (eq :instance
                       (sb-mop:slot-definition-allocation sd)))
          (push (slot-value o slot-name) ret)
          (push slot-name ret))
        finally (return ret)))

(defun struct-slots-and-values (object)
  "List of slot names followed by values for structure object"
  (let ((result nil)
        (slots 
          (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))))
    (loop for slot in slots do
         (push (slot-value object slot) result)
         (push slot result))
    result))
