;;; sugar.lisp --- Syntactic Sugar Macros

;; Utility macros to make our code a bit more sweet.

;;; Code:
(in-package :std/macs)

;; from jackdaniel's Dynamic Slots, see also META/DYNAMIC
(defmacro dlet (bindings &body body)
  "LET form -> PROGV form."
  (loop for (var val) in bindings
        collect var into vars
        collect val into vals
        finally (return `(progv (list ,@vars) (list ,@vals)
                           ,@body))))

(defun without-props (plist props)
  (loop :for (options value) :on plist :by #'cddr
        :append (unless (member options props)
                  (list options value))))

;; TODO 2024-10-24: 
;; (defmacro defclass* (name direct-superclasses direct-slots &rest opts))

;; Based on INCONGRUENT-METHODS:DEFINE-CLASS
(defmacro define-class (name direct-superclasses direct-slots &body body)
  "Like DEFCLASS but with the forms in BODY acting as simplified method
definitions."
  (with-gensyms (self)
    (labels ((slot-definition (x)
               (if (listp x)
                   (cons (first x)
                         (without-props (rest x)
                           '(:reader :writer :accessor)))
                   x))

             (slot-accessor-definition (x)
               (destructuring-bind (slot-name &rest options) x
                 (loop :for (options value) :on options :by #'cddr
                       :append
                       (case options
                         (:accessor
                          `((defmethod ,value ((,self ,name))
                              (slot-value ,self ',slot-name))
                            (defmethod (setf ,value)
                                (new (,self ,name))
                              (setf (slot-value ,self ',slot-name) new))))
                         (:reader
                          `((defmethod ,value ((,self ,name))
                              (slot-value ,self ',slot-name))))
                         (:writer
                          `((defmethod (setf ,value)
                                (new (,self ,name))
                              (setf (slot-value ,self ',slot-name) new))))))))

             (method-definition (definition)
               (destructuring-bind (method-name lambda-list &rest body)
                   definition
                 (if (listp method-name)
                     `(define-class-method ,method-name (,(first lambda-list)
                                                         (,(intern "SELF") ,name)
                                                         ,@(rest lambda-list))
                        ,@body)
                     `(define-class-method ,method-name ((,(intern "SELF") ,name)
                                                         ,@lambda-list)
                        ,@body)))))

      `(progn
         (defclass ,name ,direct-superclasses
           ,(mapcar #'slot-definition direct-slots))
         ,@(mapcan #'slot-accessor-definition
                   (remove-if-not #'listp direct-slots))
         ,@(mapcar #'method-definition body)))))
