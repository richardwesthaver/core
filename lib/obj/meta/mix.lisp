;;; mix.lisp --- Dynamic Mixin Metaclasses

;; Originally taken from StumpWM (dynamic-mixins.lisp)

;;; Commentary:

#|
mixins are for simple, dynamic class combinations:

(defclass a () ())                                            
(defclass b () ())                                            
(defclass c () ())                                            

(make-instance (mix 'a 'b)) ;; => #<MIXIN-OBJECT (A B)>       

(let ((a (make-instance 'a)))                                 
  (ensure-mix a 'b 'c)      ;; => #<MIXIN-OBJECT (B C A)>     
  (delete-from-mix a 'a)    ;; => #<MIXIN-OBJECT (B C)>       
  (delete-from-mix a 'c))   ;; => #<B>                        

This allows objects to be mixed and updated without manually defining many
permutations, at the cost of runtime dispatch.
|#

;; TODO 2026-03-27: compile-time mixins - may flag for elision at save-time

;;; Code:
(in-package :obj/meta/mix)

(defvar *mixin-classes* (make-hash-table :test 'equal))

(defclass mixin-class (standard-class)
  ((classes :initform nil :initarg :classes :accessor mixin-classes)))

(defmethod sb-mop:validate-superclass ((class mixin-class) (super standard-class)) t)

(defmethod print-object ((o mixin-class) stream)
  (with-slots (classes) o
    (print-unreadable-object (o stream :identity t)
      (format stream "~S ~S"
              (or (class-name o) 'mixin-class)
              (mapcar #'class-name classes)))))

(defclass mixin-object () ())

(defstruct mix-list (list nil))

(defun %find-class (name-or-class)
  (etypecase name-or-class
    (symbol (find-class name-or-class))
    (class name-or-class)))

(defun %mix (object-or-class &rest class-list)
  "Create a MIX-LIST for MAKE-INSTANCE.  The first element may be an
instance; further elements must be class names or classes."
  (let ((class0 (typecase object-or-class
                  (symbol (list (find-class object-or-class)))
                  (mixin-object
                   (slot-value (class-of object-or-class) 'classes))
                  (t (list (class-of object-or-class))))))
    (make-mix-list
     :list (sort (remove-duplicates
                  (append (mapcar #'%find-class class-list)
                          class0))
                 'symbol-before-p
                 :key 'class-name))))

(defun mix (&rest classes)
  (make-mix-list :list (sort (remove-duplicates (mapcar #'%find-class classes))
                             'symbol-before-p
                             :key 'class-name)))

(defun set-superclasses (class list)
  (reinitialize-instance class :direct-superclasses list))

(defun define-mixin (mix-list)
  (let ((new-class (make-instance 'mixin-class
                     :classes (mix-list-list mix-list))))
    (handler-case
        (progn
          (set-superclasses new-class (list* (find-class 'mixin-object)
                                             (mix-list-list mix-list))))
      (error (e)
        (set-superclasses new-class nil)
        (error e)))
    (setf (gethash (mix-list-list mix-list) *mixin-classes*)
          new-class)))

(defun ensure-mixin (mix-list)
  (if (cdr (mix-list-list mix-list))
      (if-let ((class (gethash (mix-list-list mix-list)
                               *mixin-classes*)))
        class
        (define-mixin mix-list))
      (car (mix-list-list mix-list))))

(defun ensure-mix (object &rest classes)
  (let ((new-class (ensure-mixin (apply #'%mix object classes))))
    (change-class object new-class)))

(defun delete-from-mix (object &rest classes)
  (if (typep object 'mixin-object)
      (let* ((classes (mapcar #'%find-class classes))
             (old-classes (slot-value (class-of object) 'classes))
             (new-classes (remove-if (lambda (x) (member (%find-class x) classes))
                                     old-classes))
             (new-class (if (cdr new-classes)
                            (ensure-mixin (apply #'mix new-classes))
                            (car new-classes))))
        (change-class object new-class))
      object))

(defmethod make-instance ((items mix-list) &rest initargs &key &allow-other-keys)
  (apply #'make-instance (ensure-mixin items) initargs))

;;; Protocol
(defgeneric replace-class (object new-class &rest initargs))

(defgeneric replace-class-in-mixin (object new-class old-class &rest initargs)
  (:method ((object standard-object) n o &rest rest)
    (declare (ignore o))
    (apply #'change-class object n rest)))

;;; Sorting
(defvar *class-ordering-rules* nil
  "A plist of rules for how to order classes for mixing. Keys are the class
names. Rules have the following shape:

(:before ((string-1 . package-designator-1)
          (string-2 . package-designator-2)
          ...
          (string-n . package-designator-n))
 :after ((string-1 . package-designator-1)
         (string-2 . package-designator-2)
         ...
         (string-n . package-designator-n)))")

(defun set-mix-rule (symbol before after)
  "Add or replace a class ordering rule for SYMBOL."
  (setf (getf *class-ordering-rules* symbol) (list :before before :after after)))

(defun symbol-ordering-rules (symbol)
  (getf *class-ordering-rules* symbol))

(defun symbol-ordering-rules-before-list (symbol &optional rules)
  (getf (or rules (symbol-ordering-rules symbol)) :before))

(defun symbol-ordering-rules-after-list (symbol &optional rules)
  (getf (or rules (symbol-ordering-rules symbol)) :after))

(defun symbol-spec-match (symbol spec)
  (let ((p (find-package (cdr spec))))
    (when p
      (eq (find-symbol (string (car spec)) p)
          symbol))))

(defun symbol-before-p (s1 s2)
  "Return truthy if S1 should be before S2."
  (or (find s2 (symbol-ordering-rules-before-list s1) :test #'symbol-spec-match)
      (find s1 (symbol-ordering-rules-after-list s2) :test #'symbol-spec-match)))

(defun symbol-after-p (s1 s2)
  "Return truthy if S1 should be after S2."
  (or (find s2 (symbol-ordering-rules-after-list s1) :test #'symbol-spec-match)
      (find s1 (symbol-ordering-rules-before-list s2) :test #'symbol-spec-match)))
