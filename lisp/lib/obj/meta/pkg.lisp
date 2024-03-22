;;; obj/meta/pkg.lisp --- Meta-objects

;;

;;; Code:
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

(defpackage :obj/meta/stealth
  (:use :cl :std))

(defpackage :obj/meta/typed
  (:use :cl :std))

(defpackage :obj/meta/filtered
  (:use :cl :std)
  (:export
   :define-filtered-function :filtered :filtered-function :filtered-method
   :generic-function-filter-expression :generic-function-filters :method-filter :simple-filtered-function))

(defpackage :obj/meta/sealed
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
  (:use :cl :std)
  (:import-from :sb-int :gensymify)
  (:import-from :sb-walker :macroexpand-all)
  (:import-from :obj/meta/sealed
   :method-properties :validate-method-property :seal-domain :domain
   :sealed-domains :compute-static-call-signatures :static-call-signature :static-call-signature-types
   :static-call-signature-prototypes :externalizable-object-p :sealable-class :sealable-generic-function
   :sealable-standard-generic-function :potentially-sealable-method :potentially-sealable-standard-method)
  (:export :fast-generic-function :fast-method :inlineable))

(defpackage :obj/meta/lazy
  (:use :cl :std))

(defpackage :obj/meta/overloaded
  (:use :cl :std))
