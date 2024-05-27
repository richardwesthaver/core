;;; obj/meta/pkg.lisp --- Meta-objects

;;

;;; Code:
(defpackage :obj/meta/stealth
  (:use :cl :std :obj/meta :sb-mop))

(defpackage :obj/meta/typed
  (:use :cl :std :obj/meta :sb-mop))

(defpackage :obj/meta/filtered
  (:use :cl :std :obj/meta :sb-mop)
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
  (:use :cl :std :obj/meta/sealed :obj/meta)
  (:import-from :sb-int :gensymify)
  (:import-from :sb-walker :macroexpand-all)
  (:export :fast-generic-function :fast-method :inlineable))

(defpackage :obj/meta/lazy
  (:use :cl :std :obj/meta))

(defpackage :obj/meta/overloaded
  (:use :cl :std :obj/meta))

(defpackage :obj/meta/storable
  (:use :cl :std :obj/meta :obj/id)
  (:export
   :storable-class :initialize-storable-class
   :storable-slot-mixin :storable-direct-slot-definition
   :storable-effective-slot-definition))
  
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
