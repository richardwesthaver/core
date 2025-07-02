;;; obj/meta/sealed.lisp --- Sealed Meta-objects

;; see https://github.com/marcoheisig/sealable-metaobjects

;;; Commentary:

;; From the sealable-metaobjects readme:
#|
The goal is to inline a generic function under certain circumstances. These circumstances are:

1 It is possible to statically determine the generic function being called.
2 This generic function is sealed, i.e., it is an instance of SEALABLE-GENERIC-FUNCTION that has previously been passed
  to the function SEAL-GENERIC-FUNCTION.
3 This sealed generic function has at least one sealed method, i.e., a method of type POTENTIALLY-SEALABLE-METHOD that
  specializes, on each relevant argument, on a built-in or sealed class, or an eql specializer whose object is an
  instance of a built-in or sealed class.
4 It must be possible to determine, statically, that the types of all arguments in a specializing position uniquely
  determine the list of applicable methods.

Examples

The following examples illustrate how sealable metaobjects can be used. Each example code can be evaluated as-is.
However, for actual use, we recommend the following practices:

* Sealable generic functions should be defined in a separate file that is loaded early. If this is not done, its methods
  may not use the correct method-class. (An alternative is to specify the method class of each method explicitly).
* Metaobject sealing should be the very last step when loading a project. Ideally, all calls to SEAL-GENERIC-FUNCTION
  should be in a separate file that ASDF loads last. This way, sealing can also be disabled conveniently, e.g., to
  measure whether sealing actually improves performance (Which you should do!).

Generic Plus

This example shows how one can implement a generic version of cl:+.

(defgeneric generic-binary-+ (a b)
  (:generic-function-class fast-generic-function))

(defmethod generic-binary-+ ((a number) (b number))
  (+ a b))

(defmethod generic-binary-+ ((a character) (b character))
  (+ (char-code a)
     (char-code b)))

(sealable-metaobjects:seal-domain #'generic-binary-+ '(number number))
(sealable-metaobjects:seal-domain #'generic-binary-+ '(character character))

(defun generic-+ (&rest things)
  (cond ((null things) 0)
        ((null (rest things)) (first things))
        (t (reduce #'generic-binary-+ things))))

(define-compiler-macro generic-+ (&rest things)
  (cond ((null things) 0)
        ((null (rest things)) (first things))
        (t
         (flet ((symbolic-generic-binary-+ (a b)
                  `(generic-binary-+ ,a ,b)))
           (reduce #'symbolic-generic-binary-+ things)))))

You can quickly verify that this new operator is as efficient as cl:+:

(defun triple-1 (x)
  (declare (single-float x))
  (+ x x x))

(defun triple-2 (x)
  (declare (single-float x))
  (generic-+ x x x))

;;; Both functions should compile to the same assembler code.
(disassemble #'triple-1)
(disassemble #'triple-2)

Yet, other than cl:+, generic-+ can be extended by the user, just like a regular generic function. The only restriction
is that new methods must not interfere with the behavior of methods that specialize on sealed types only.

Generic Find

This example illustrates how one can implement a fast, generic version of cl:find.

(defgeneric generic-find (item sequence &key test)
  (:generic-function-class fast-generic-function))

(defmethod generic-find (elt (list list) &key (test #'eql))
  (and (member elt list :test test)
       t))

(defmethod generic-find (elt (vector vector) &key (test #'eql))
  (cl:find elt vector :test test))

(seal-domain #'generic-find '(t list))
(seal-domain #'generic-find '(t vector))

(defun small-prime-p (x)
  (generic-find x '(2 3 5 7 11)))

;; The call to GENERIC-FIND should have been replaced by a direct call to
;; the appropriate effective method.
(disassemble #'small-prime-p)

|#
;;; Code:
(in-package :obj/meta/sealed)

(defun %starts-with (item)
  (lambda (sequence)
    (typecase sequence
      (list (eql (first sequence) item))
      (sequence (eql (elt sequence 0) item))
      (otherwise nil))))

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

(defgeneric ensure-specializer (specializer-designator)
  (:method ((class class))
    class)
  (:method ((symbol symbol))
    (or (find-class symbol nil)
        (call-next-method)))
  (:method ((cons cons))
    (if (typep cons '(cons (eql eql) (cons t null)))
        (intern-eql-specializer (second cons))
        (call-next-method)))
  (:method ((object t))
    (error "~@<~S is not a specializer, or a type designator that ~
                can be converted to a specializer.~:@>"
           object)))

(defgeneric specializer-type (specializer)
  (:method ((class class))
    (class-name class))
  (:method ((eql-specializer eql-specializer))
    `(eql ,(eql-specializer-object eql-specializer))))

(defgeneric specializer-prototype (specializer &optional excluded-specializers)
  (:documentation
   "Returns an object that is of the type indicated by SPECIALIZER, but not
of any of the types indicated the optionally supplied
EXCLUDED-SPECIALIZERS.  Returns a secondary value of T if such an object
could be determined, and NIL if no such object was found.

Examples:
 (specializer-prototype
   (find-class 'double-float))
 => 5.0d0, T

 (specializer-prototype
   (find-class 'double-float)
   (list (intern-eql-specializer 5.0d0)))
 => 6.0d0, T

 (specializer-prototype
   (find-class 'real)
   (list (find-class 'rational) (find-class 'float)))
 => NIL, NIL
"))

(defgeneric specializer-direct-superspecializers (specializer)
  (:method ((class class))
    (class-direct-superclasses class))
  (:method ((eql-specializer eql-specializer))
    (list
     (class-of
      (eql-specializer-object eql-specializer)))))

(defgeneric specializer-intersectionp (specializer-1 specializer-2)
  (:method ((class-1 class) (class-2 class))
    (multiple-value-bind (disjointp success)
        (subtypep `(and ,class-1 ,class-2) nil)
      (assert success)
      (not disjointp)))
  (:method ((class class) (eql-specializer eql-specializer))
    (typep (eql-specializer-object eql-specializer) class))
  (:method ((eql-specializer eql-specializer) (class class))
    (typep (eql-specializer-object eql-specializer) class))
  (:method ((eql-specializer-1 eql-specializer) (eql-specializer-2 eql-specializer))
    (eql (eql-specializer-object eql-specializer-1)
         (eql-specializer-object eql-specializer-2))))

(defgeneric specializer-subsetp (specializer-1 specializer-2)
  (:method ((class-1 class) (class-2 class))
    (values (subtypep class-1 class-2)))
  (:method ((class class) (eql-specializer eql-specializer))
    (subtypep class (specializer-type eql-specializer)))
  (:method ((eql-specializer eql-specializer) (class class))
    (typep (eql-specializer-object eql-specializer) class))
  (:method ((eql-specializer-1 eql-specializer) (eql-specializer-2 eql-specializer))
    (eql (eql-specializer-object eql-specializer-1)
         (eql-specializer-object eql-specializer-2))))

;;; Working with domains.

(defgeneric ensure-domain (domain-designator))

(defgeneric method-domain (method))

(defgeneric domain-specializers (domain))

(defgeneric domain-arity (domain))

(defgeneric domain-equal (domain-1 domain-2))

(defgeneric domain-intersectionp (domain-1 domain-2))

(defgeneric domain-subsetp (domain-1 domain-2))

;;; Checking for sealability.

(defgeneric metaobject-sealable-p (metaobject)
  (:method ((class class)) (eql class (find-class t)))
  (:method ((generic-function generic-function)) nil)
  (:method ((method method)) nil)
  (:method ((built-in-class built-in-class)) t)
  (:method ((structure-class structure-class)) t)
  (:method ((system-class sb-pcl:system-class)) t))

(defgeneric class-sealable-p (class)
  (:method ((class class))
    (metaobject-sealable-p class)))

(defgeneric generic-function-sealable-p (generic-function)
  (:method ((generic-function generic-function))
    (metaobject-sealable-p generic-function)))

(defgeneric method-sealable-p (method)
  (:method ((method method))
    (metaobject-sealable-p method)))

(defgeneric specializer-sealable-p (specializer)
  (:method ((class class))
    (class-sealable-p class))
  (:method ((eql-specializer eql-specializer))
    (class-sealable-p
     (class-of
      (eql-specializer-object eql-specializer)))))

;;; Checking for sealed-ness.

(defgeneric metaobject-sealed-p (metaobject)
  (:method ((class class)) (eql class (find-class t)))
  (:method ((generic-function generic-function)) nil)
  (:method ((method method)) nil)
  (:method ((built-in-class built-in-class)) t)
  (:method ((structure-class structure-class)) t)
  (:method ((system-class sb-pcl:system-class)) t))

(defgeneric class-sealed-p (class)
  (:method ((class class))
    (metaobject-sealed-p class)))

(defgeneric generic-function-sealed-p (generic-function)
  (:method ((generic-function generic-function))
    (metaobject-sealed-p generic-function)))

(defgeneric method-sealed-p (method)
  (:method ((method method))
    (metaobject-sealed-p method)))

(defgeneric specializer-sealed-p (specializer)
  (:method ((class class))
    (class-sealed-p class))
  (:method ((eql-specializer eql-specializer))
    (specializer-sealed-p
     (class-of
      (eql-specializer-object eql-specializer)))))

;;; Sealing of metaobjects.

(defgeneric seal-metaobject (metaobject)
  ;; Invoke primary methods on SEAL-METAOBJECT at most once.
  (:method :around ((metaobject t))
    (unless (metaobject-sealed-p metaobject)
      (call-next-method)))
  ;; Signal an error if the default primary method is reached.
  (:method ((metaobject t))
    (error "Cannot seal the metaobject ~S." metaobject))
  (:method :before ((class class))
    ;; Class sealing implies finalization.
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    ;; A sealed class must have sealed superclasses.
    (loop for class in (rest (class-precedence-list class))
          until (member class *standard-metaobjects*)
          do (seal-class class))))

(defgeneric seal-class (class)
  ;; Invoke primary methods on SEAL-CLASS at most once.
  (:method :around ((class class))
    (unless (class-sealed-p class)
      (call-next-method)))
  (:method ((symbol symbol))
    (seal-metaobject (find-class symbol)))
  (:method ((class class))
    (seal-metaobject class)))

(defgeneric seal-generic-function (generic-function)
  ;; Invoke primary methods on SEAL-GENERIC-FUNCTION at most once.
  (:method :around ((generic-function generic-function))
    (unless (generic-function-sealed-p generic-function)
      (call-next-method)))
  (:method ((generic-function generic-function))
    (seal-metaobject generic-function)))

(defgeneric seal-method (method)
  ;; Invoke primary methods on SEAL-METHOD at most once.
  (:method :around ((method method))
    (unless (method-sealed-p method)
      (call-next-method)))
  (:method ((method method))
    (seal-metaobject method)))

(defgeneric seal-domain (generic-function domain))

(defgeneric seal-specializer (specializer)
  (:method ((class class))
    (seal-class class))
  (:method ((eql-specializer eql-specializer))
    (seal-class
     (class-of
      (eql-specializer-object eql-specializer)))))

;;; Method properties

(defgeneric method-properties (method)
  (:method ((method method))
    '()))

(defgeneric validate-method-property (method method-property)
  (:method ((method method) (method-property t))
    nil))

;;; Miscellaneous

(defgeneric sealed-domains (generic-function)
  (:method ((generic-function generic-function))
    '()))

(defgeneric compute-static-call-signatures (generic-function domain))

(defgeneric externalizable-object-p (object)
  ;; Built-in objects are usually externalizable.
  (:method ((object t))
    (typep (class-of object) 'built-in-class))
  ;; Functions are not externalizable by definition.
  (:method ((function function))
    nil)
  ;; Structure objects may be externalizable even without an appropriate
  ;; method on MAKE-LOAD-FORM.
  (:method ((structure-object structure-object))
    ;; TODO: Returning T here is a bit bold.  Actually we'd have to check
    ;; whether each slot of the structure has a value that is
    ;; externalizable.
    t)
  ;; Standard objects are only externalizable if they have an appropriate
  ;; method on MAKE-LOAD-FORM.
  (:method ((standard-object standard-object))
    (and (make-load-form standard-object) t)))

(defclass domain ()
  ((%specializers
    :initform (required-argument :specializers)
    :initarg :specializers
    :reader domain-specializers)
   (%arity
    :initform (required-argument :arity)
    :initarg :arity
    :reader domain-arity)))

(defmethod print-object ((domain domain) stream)
  (print-unreadable-object (domain stream :type t)
    (format stream "~{~S~^ ~}"
            (mapcar #'specializer-type (domain-specializers domain)))))

(defun make-domain (specializers &aux (arity (list-length specializers)))
  (dolist (specializer specializers)
    (check-type specializer specializer))
  (make-instance 'domain
    :specializers specializers
    :arity arity))

(defmethod ensure-domain ((domain domain))
  domain)

(defmethod ensure-domain ((sequence sequence))
  (make-domain
   (map 'list #'ensure-specializer sequence)))

(defmethod method-domain ((method method))
  (make-domain (method-specializers method)))

(defmethod domain-equal
    ((domain-1 domain)
     (domain-2 domain))
  (and (= (domain-arity domain-1)
          (domain-arity domain-2))
       (every #'eq
              (domain-specializers domain-1)
              (domain-specializers domain-2))))

(defmethod domain-intersectionp
    ((domain-1 domain)
     (domain-2 domain))
  (assert (= (domain-arity domain-1)
             (domain-arity domain-2)))
  (every #'specializer-intersectionp
         (domain-specializers domain-1)
         (domain-specializers domain-2)))

(defmethod domain-subsetp
    ((domain-1 domain)
     (domain-2 domain))
  (assert (= (domain-arity domain-1)
             (domain-arity domain-2)))
  (every #'specializer-subsetp
         (domain-specializers domain-1)
         (domain-specializers domain-2)))

(defclass sealable-metaobject-mixin ()
  ((%sealed-p :initform nil :reader metaobject-sealed-p)))

(defmethod metaobject-sealable-p ((metaobject sealable-metaobject-mixin))
  t)

(defmethod seal-metaobject ((metaobject sealable-metaobject-mixin))
  (setf (slot-value metaobject '%sealed-p) t))

;;; It is an error to change the class of a sealed metaobject.
(defmethod change-class :around
    ((metaobject sealable-metaobject-mixin) new-class &key &allow-other-keys)
  (declare (ignore new-class))
  (if (metaobject-sealed-p metaobject)
      (error "Attempt to change the class of the sealed metaobject ~S."
             metaobject)
      (call-next-method)))

;;; It is an error to change any object's class to a sealed metaobject.
(defmethod update-instance-for-different-class :around
    (previous (current sealable-metaobject-mixin) &key &allow-other-keys)
  (error "Attempt to change the class of ~S to the sealable metaobject ~S."
         previous (class-of current)))

;;; Attempts to reinitialize a sealed metaobject are silently ignored.
(defmethod reinitialize-instance :around
    ((metaobject sealable-metaobject-mixin) &key &allow-other-keys)
  (if (metaobject-sealed-p metaobject)
      metaobject
      (call-next-method)))

;;; It is an error to change the class of an instance of a sealable
;;; metaobject.

(defclass sealable-metaobject-instance (t)
  ())

(defmethod change-class :around
    ((instance sealable-metaobject-instance) new-class &key &allow-other-keys)
  (declare (ignore new-class))
  (error "Attempt to change the class of the sealable metaobject instance ~S."
         instance))

(defmethod shared-initialize
    ((instance sealable-metaobject-mixin)
     (slot-names (eql t))
     &rest initargs
     &key direct-superclasses)
  (unless (every #'class-sealable-p direct-superclasses)
    (error "~@<The superclasses of a sealable metaobject must be sealable. ~
               The superclass ~S violates this restriction.~:@>"
           (find-if-not #'class-sealable-p direct-superclasses)))
  (apply #'call-next-method instance slot-names
         :direct-superclasses
         (adjoin (find-class 'sealable-metaobject-instance) direct-superclasses)
         initargs))

(defclass sealable-class (sealable-metaobject-mixin class)
  ())

;;; There is no portable way to add options to a method.  So instead, we
;;; allow programmers to declare METHOD-PROPERTIES.
;;;
;;; Example:
;;;
;;; (defmethod foo (x y)
;;;   (declare (method-properties inline))
;;;   (+ x y))

(declaim (declaration method-properties))

(defclass potentially-sealable-method (sealable-metaobject-mixin method)
  ((%method-properties
    :initarg .method-properties.
    :accessor method-properties
    :initform '())))

(defmethod shared-initialize :after
    ((psm potentially-sealable-method)
     slot-names &key ((.method-properties. method-properties) '()) &allow-other-keys)
  (declare (ignore slot-names))
  (dolist (method-property method-properties)
    (unless (validate-method-property psm method-property)
      (error "~@<~S is not a valid method property for the method ~S.~@:>"
             method-property psm))))

;;; Track all properties that have been declared in the body of the method
;;; lambda, and make them accessible as METHOD-PROPERTIES of that method.
(defmethod make-method-lambda :around
    ((gf generic-function)
     (psm potentially-sealable-method)
     lambda
     environment)
  (declare (ignore environment))
  (multiple-value-bind (method-lambda initargs)
      (call-next-method)
    (values
     method-lambda
     (list* '.method-properties.
            (let* ((declare-forms (remove-if-not (%starts-with 'declare) lambda))
                   (declarations (apply #'append (mapcar #'rest declare-forms))))
              (reduce #'union (remove-if-not (%starts-with 'method-properties) declarations)
                      :key #'rest
                      :initial-value '()))
            initargs))))

(defmethod metaobject-sealable-p ((psm potentially-sealable-method))
  (every #'specializer-sealed-p (method-specializers psm)))

(defmethod seal-metaobject :before ((psm potentially-sealable-method))
  (mapcar #'seal-specializer (method-specializers psm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived Classes

(defclass potentially-sealable-standard-method
    (standard-method potentially-sealable-method)
  ())

(defclass sealable-generic-function (sealable-metaobject-mixin generic-function)
  ((%sealed-domains
    :initform '()
    :type list
    :reader sealed-domains
    :writer (setf %sealed-domains)))
  (:default-initargs
   :method-class (find-class 'potentially-sealable-method))
  (:metaclass funcallable-standard-class))

;;; Check that the supplied domain is sane.
(defmethod seal-domain
    ((sgf sealable-generic-function)
     (domain t))
  (seal-domain sgf (ensure-domain domain)))

(defmethod seal-domain :around
    ((sgf sealable-generic-function)
     (domain domain))
  ;; Ensure that we don't seal any domain more than once.
  (unless (find domain (sealed-domains sgf) :test #'domain-equal)
    (call-next-method sgf domain)))

;;; Ensure that the generic function is sealed, and that the newly sealed
;;; domain is disjoint from other domains.
(defmethod seal-domain :before
    ((sgf sealable-generic-function)
     (domain domain))
  ;; Ensure that the length of the domain matches the number of mandatory
  ;; arguments of the generic function.
  (unless (= (domain-arity domain)
             (length (generic-function-argument-precedence-order sgf)))
    (error "~@<Cannot seal the domain ~S with arity ~R ~
               of the generic function ~S with arity ~R.~@:>"
           (mapcar #'specializer-type (domain-specializers domain))
           (domain-arity domain)
           (generic-function-name sgf)
           (length (generic-function-argument-precedence-order sgf))))
  ;; Attempt to seal the supplied generic function.
  (seal-generic-function sgf)
  ;; Ensure that the domain does not intersect any existing sealed domains.
  (dolist (existing-domain (sealed-domains sgf))
    (when (domain-intersectionp domain existing-domain)
      (error "~@<Cannot seal the domain ~S of the generic function ~S, ~
               because it intersects with the existing domain ~S.~@:>"
             (mapcar #'specializer-type domain)
             sgf
             (mapcar #'specializer-type existing-domain)))))

;;; Add a new sealed domain.
(defmethod seal-domain
    ((sgf sealable-generic-function)
     (domain domain))
  (dolist (method (generic-function-methods sgf))
    (when (domain-intersectionp (method-domain method) domain)
      (unless (domain-subsetp (method-domain method) domain)
        (error "~@<The method ~S with specializers ~S is only partially ~
                   within the sealed domain ~S.~:@>"
               method
               (mapcar #'specializer-type (method-specializers method))
               (mapcar #'specializer-type (domain-specializers domain))))
      (seal-method method)))
  (setf (%sealed-domains sgf)
        (cons domain (sealed-domains sgf))))

;;; Skip the call to add-method if the list of specializers is equal to
;;; that of an existing, sealed method.
(defmethod add-method :around
    ((sgf sealable-generic-function)
     (psm potentially-sealable-method))
  (dolist (method (generic-function-methods sgf))
    (when (and (method-sealed-p method)
               (equal (method-specializers psm)
                      (method-specializers method)))
      (return-from add-method psm)))
  (call-next-method))

;;; Ensure that the method to be added is disjoint from all sealed domains.
(defmethod add-method :before
    ((sgf sealable-generic-function)
     (psm potentially-sealable-method))
  (dolist (domain (sealed-domains sgf))
    (when (domain-intersectionp domain (method-domain psm))
      (error "~@<Cannot add the method ~S with specializers ~S to ~
                 the sealed generic function ~S, because it intersects ~
                 with the existing sealed domain ~S.~:@>"
             psm (method-specializers psm)
             sgf (mapcar #'specializer-type (domain-specializers domain))))))

;;; Ensure that the method to be removed is disjoint from all sealed domains.
(defmethod remove-method :before
    ((sgf sealable-generic-function)
     (psm potentially-sealable-method))
  (dolist (domain (sealed-domains sgf))
    (when (domain-intersectionp domain (method-domain psm))
      (error "~@<Cannot remove the method ~S with specializers ~S from ~
                 the sealed generic function ~S, because it intersects ~
                 with the existing sealed domain ~S.~:@>"
             psm (method-specializers psm)
             sgf (mapcar #'specializer-type (domain-specializers domain))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived Classes

(defclass sealable-standard-generic-function
    (standard-generic-function sealable-generic-function)
  ()
  (:default-initargs
   :method-class (find-class 'potentially-sealable-standard-method))
  (:metaclass funcallable-standard-class))

;;; Finding a suitable prototype for eql specializers is easy.
(defmethod specializer-prototype ((eql-specializer eql-specializer)
                                  &optional excluded-specializers)
  (if (member eql-specializer excluded-specializers)
      (values nil nil)
      (values (eql-specializer-object eql-specializer) t)))

(defun eql-specializer-p (object)
  (typep object 'eql-specializer))

(defmethod specializer-prototype ((class class) &optional excluded-specializers)
  (let* ((excluded-non-eql-specializers (remove-if #'eql-specializer-p excluded-specializers))
         (excluded-eql-specializers (remove-if-not #'eql-specializer-p excluded-specializers))
         (excluded-objects (mapcar #'eql-specializer-object excluded-eql-specializers))
         (excluded-types (mapcar #'specializer-type excluded-non-eql-specializers)))
    (map-class-prototypes
     (lambda (prototype)
       ;; The prototype must not be a member of the excluded objects.
       (when (not (member prototype excluded-objects))
         ;; The prototype must not be of one of the excluded types.
         (when (notany
                (lambda (excluded-type)
                  (typep prototype excluded-type))
                excluded-types)
           (return-from specializer-prototype (values prototype t)))))
     class)
    (values nil nil)))

;;; The difficult part is to find suitable prototypes for specializers that
;;; are classes.  Ideally, we want several prototypes for each class, such
;;; that we can avoid collisions with excluded specializers.  Our technique
;;; is to find prototypes from two sources - the value returned by the MOP
;;; function CLASS-PROTOTYPE, and manually curated lists of prototypes for
;;; each class, which we store in the hash table *CLASS-PROTOTYPES*.

(defvar *class-prototypes* (make-hash-table :test #'eq))

(defun map-class-prototypes (function class)
  (let ((visited-classes (make-hash-table :test #'eq)))
    (labels ((visit-class (class)
               (unless (gethash class visited-classes)
                 (setf (gethash class visited-classes) t)
                 (loop for prototype in (gethash class *class-prototypes* '()) do
                   (funcall function prototype))
                 (mapc #'visit-class (class-direct-subclasses class))
                 ;; CLASS-PROTOTYPE is difficult to handle...
                 (when (class-finalized-p class)
                   (let ((prototype (class-prototype class)))
                     ;; Surprisingly, some implementations don't always
                     ;; return a CLASS-PROTOTYPE that is an instance of the
                     ;; given class.  So we only scan the prototype if it is
                     ;; actually valid.
                     (when (typep prototype class)
                       (funcall function prototype)))))))
      (visit-class class))))

(defun register-class-prototype (prototype)
  (pushnew prototype (gethash (class-of prototype) *class-prototypes* '())
           :test #'equalp))

;; Register list prototypes.
(register-class-prototype '(.prototype.))
(register-class-prototype nil)

(defparameter *array-element-types*
  (remove-duplicates
   (mapcar #'upgraded-array-element-type
           (append '(short-float single-float double-float long-float base-char character t)
                   '((complex short-float)
                     (complex single-float)
                     (complex double-float)
                     (complex long-float))
                   (loop for bits from 1 to 64
                         collect `(unsigned-byte ,bits)
                         collect `(signed-byte ,bits))))
   :test #'equal))

(defun array-initial-element (element-type)
  (cond ((subtypep element-type 'number)
         (coerce 0 element-type))
        ((subtypep element-type 'character)
         (coerce #\0 element-type))
        (t t)))

;; Register vector and array prototypes.
(loop for adjustable in '(nil t) do
  (loop for fill-pointer in '(nil t) do
    (loop for dimensions in '(() (2) (2 2)) do
      (loop for element-type in *array-element-types* do
        (let ((storage-vector
                (make-array (reduce #'* dimensions)
                            :element-type element-type
                            :initial-element (array-initial-element element-type))))
          (register-class-prototype
           (make-array dimensions
                       :adjustable adjustable
                       :fill-pointer (and (= 1 (length dimensions)) fill-pointer)
                       :element-type element-type
                       :displaced-to storage-vector))
          (register-class-prototype
           (make-array dimensions
                       :adjustable adjustable
                       :fill-pointer (and (= 1 (length dimensions)) fill-pointer)
                       :element-type element-type
                       :initial-element (array-initial-element element-type))))))))

;; Register integer and rational prototypes.
(loop for integer in '(19 1337 1338 91676) do
  (register-class-prototype (+ integer))
  (register-class-prototype (- integer)))
(loop for bits = 1 then (* bits 2) until (>= bits 512)
      for value = (expt 2 bits) do
        (loop for value in (list (1+ value) value (1- value)) do
          (register-class-prototype value)
          (register-class-prototype (- value))
          (register-class-prototype (/ value 17))))

;; Register float and complex float prototypes.
(register-class-prototype pi)
(register-class-prototype (- pi))
(register-class-prototype (exp 1S0))
(register-class-prototype (exp 1F0))
(register-class-prototype (exp 1D0))
(register-class-prototype (exp 1L0))
(mapcar #'register-class-prototype
        (list most-positive-short-float
              most-positive-single-float
              most-positive-double-float
              most-positive-long-float
              most-negative-short-float
              most-negative-single-float
              most-negative-double-float
              most-positive-long-float
              short-float-epsilon
              single-float-epsilon
              double-float-epsilon
              long-float-epsilon
              short-float-negative-epsilon
              single-float-negative-epsilon
              double-float-negative-epsilon
              long-float-negative-epsilon))
(loop for base in '(-0.7L0 -0.1L0 -0.0L0 +0.0L0 +0.1L0 +0.7L0) do
  (loop for fp-type in '(short-float single-float double-float long-float) do
    (loop for exponent in '(1 2 3 5 7 23 99) do
      (let ((float (scale-float (coerce base fp-type) exponent)))
        (register-class-prototype float)
        (register-class-prototype (complex (float 0 float) float))))))

;; Register character prototypes.
(loop for char across "The quick brown fox jumps over the lazy dog." do
  (register-class-prototype (char-downcase char))
  (register-class-prototype (char-upcase char)))
(loop for char across "0123456789!$\"'(),_-./:;?+<=>#%&*@[\\]{\|}`^~" do
  (register-class-prototype char))
(loop for char in '(#\backspace #\tab #\newline #\linefeed #\page #\return #\space #\rubout) do
  (register-class-prototype char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning About Specializer Specificity

(defclass snode ()
  (;; The specializer of an snode.
   (%specializer :initarg :specializer :accessor snode-specializer)
   ;; A (possibly empty) list of snodes for each child class or eql specializer.
   (%children :initform '() :accessor snode-children)
   ;; A list of snodes with one entry for each parent class.
   (%parents :initform '() :accessor snode-parents)
   ;; Whether the snode has already been visited.
   (%visitedp :initform nil :accessor snode-visitedp)
   ;; Whether the snode corresponds to a specializer of an existing method
   ;; or the domain.
   (%relevantp :initform nil :accessor snode-relevantp)))

(defun snode-type (snode)
  (type-specifier-and
   (specializer-type (snode-specializer snode))
   (type-specifier-not
    (apply #'type-specifier-or
           (loop for subspecializer in (snode-children snode)
                 collect
                 (specializer-type
                  (snode-specializer subspecializer)))))))

(defun snode-prototype (snode)
  (specializer-prototype
   (snode-specializer snode)
   (mapcar #'snode-specializer (snode-children snode))))

(defvar *snode-table*)

(defun specializer-snode (specializer)
  (multiple-value-bind (snode present-p)
      (gethash specializer *snode-table*)
    (if present-p
        snode
        (let ((snode (make-instance 'snode :specializer specializer)))
          (setf (gethash specializer *snode-table*) snode)
          snode))))

(defun snode-add-edge (super-snode sub-snode)
  (pushnew super-snode (snode-parents sub-snode))
  (pushnew sub-snode (snode-children super-snode))
  (values))

(defun type-prototype-pairs (specializers domain)
  (let* ((*snode-table* (make-hash-table))
         (specializer-snodes (mapcar #'specializer-snode specializers))
         (domain-snode (specializer-snode domain)))
    ;; Initialize domain and specializer snodes.
    (dolist (snode specializer-snodes)
      (setf (snode-relevantp snode) t))
    (setf (snode-relevantp domain-snode) t)
    ;; Now connect all snodes.
    (labels ((visit (current relevant)
               (unless (snode-visitedp current)
                 (setf (snode-visitedp current) t)
                 (unless (eql current domain)
                   (dolist (specializer
                            (specializer-direct-superspecializers
                             (snode-specializer current)))
                     (let ((super (specializer-snode specializer)))
                       (cond ((snode-relevantp super)
                              (snode-add-edge super relevant)
                              (visit super super))
                             (t
                              (visit super relevant)))))))))
      (mapc #'visit specializer-snodes specializer-snodes))
    ;; Finally, build all pairs.
    (let ((pairs '()))
      (loop for snode being the hash-values of *snode-table* do
        (when (snode-relevantp snode)
          (multiple-value-bind (prototype prototype-p)
              (snode-prototype snode)
            (when prototype-p
              (push (list (snode-type snode) prototype)
                    pairs)))))
      pairs)))

;;; In this file, we compute the static call signatures of a given, sealed
;;; generic function. A static call signature consists of a list of types,
;;; and a list of prototypes.  The list of types is guaranteed to be
;;; non-overlapping with the types of any other call signature.  The list
;;; of prototypes is chosen such that the list of applicable methods of
;;; these prototypes is representative for all arguments of the types of
;;; the call signature.

(defclass static-call-signature ()
  ((%types
    :initarg :types
    :reader static-call-signature-types)
   (%prototypes
    :initarg :prototypes
    :reader static-call-signature-prototypes)))

(defmethod print-object ((scs static-call-signature) stream)
  (print-unreadable-object (scs stream :type t :identity t)
    (format stream "~S ~S"
            (static-call-signature-types scs)
            (static-call-signature-prototypes scs))))

(defmethod make-load-form
    ((static-call-signature static-call-signature) &optional environment)
  (make-load-form-saving-slots
   static-call-signature
   :slot-names '(%types %prototypes)
   :environment environment))

(defmethod externalizable-object-p
    ((static-call-signature static-call-signature))
  (and
   (every #'externalizable-object-p
          (static-call-signature-types static-call-signature))
   (every #'externalizable-object-p
          (static-call-signature-prototypes static-call-signature))))

(defmethod compute-static-call-signatures
    ((sgf sealable-generic-function)
     (domain domain))
  (let* ((sealed-methods
           (remove-if-not
            (lambda (method)
              (domain-intersectionp (method-domain method) domain))
            (generic-function-methods sgf)))
         (list-of-specializers
           (mapcar #'method-specializers sealed-methods))
         (static-call-signatures '()))
    (unless (null list-of-specializers)
      (map-types-and-prototypes
       (lambda (types prototypes)
         (push (make-instance 'static-call-signature
                 :types types
                 :prototypes prototypes)
               static-call-signatures))
       ;; Transpose the list of specializers so that we operate on each
       ;; argument instead of on each method.
       (apply #'mapcar #'list list-of-specializers)
       domain))
    static-call-signatures))

(defun map-types-and-prototypes (fn specializers-list domain)
  (assert (= (length specializers-list)
             (domain-arity domain)))
  (labels ((rec (sl specializers types prototypes)
             (if (null sl)
                 (funcall fn (reverse types) (reverse prototypes))
                 (loop for (type prototype)
                         in (type-prototype-pairs
                             (first sl)
                             (first specializers))
                       do (rec (rest sl)
                               (rest specializers)
                               (cons type types)
                               (cons prototype prototypes))))))
    (rec specializers-list (domain-specializers domain) '() '())))
