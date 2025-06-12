;;; std/serde.lisp --- Basic Lisp Serializer API

;; Read/Write Lisp Objects.

;;; Commentary:

;; This package contains macros for defining a pair of functions for a
;; category of lisp types - a READ-* function and a WRITE-* function. These
;; functions operate on a storage context which we Serialize (write) and Deserialize
;; (read) values from.

;; Within the STD system we implement the API for octet vectors as well as
;; (ALIEN (* UNSIGNED-CHAR)). These are used by higher-level packages which
;; need to portably serialize lisp objects as octet vectors.

;;; Code:
(in-package :std/serde)

(eval-always
  (define-condition serde-condition () ()
    (:documentation "Default SERDE condition class."))
  (deferror serde-error (serde-condition) ()
    (:documentation "An error signaled during serialization OR deserialization.")))

(deferror serializer-error (serde-error)
  ()
  (:documentation "An error which occurs during object serialization."))

(deferror deserializer-error (serde-error) 
  ()
  (:documentation "An error which occurs during object deserialization."))

;;; Serialize
(defgeneric serializable-p (self)
  (:method ((self t)) nil)
  (:documentation "Return non-nil of object SELF is serializable."))

(defgeneric serialize (obj format &key &allow-other-keys)
  (:documentation "Serialize OBJ to FORMAT, which is a SERIALIZABLE-TYPE-DESIGNATOR."))
;;; Deserialize
(defgeneric deserializable-p (self)
  (:method ((self t)) nil)
  (:documentation "Return non-nil if object SELF is deserializable."))

(defgeneric deserialize (from format &key &allow-other-keys)
  (:documentation "Deserialize FROM into an object of type FORMAT, which is a
DESERIALIZABLE-TYPE-DESIGNATOR."))

(defgeneric ser (kind obj)
  (:documentation "Convenience function for serializing OBJ into KIND."))
(defgeneric de (kind obj)
  (:documentation "Convenience function for deserializing OBJ of type KIND."))

(defgeneric serde (from to)
  (:documentation "Point-to-point serialization.

FROM and TO should both specialize on object instances.

Calling this function requires you to initialize the arguments instead
of relying on a type-designator format and generating an object in the
method body."))

(defparameter *primitive-object-table*
  (let ((tbl (make-hash-table)))
    (dolist (obj *primitive-objects* tbl)
      (setf (gethash (primitive-object-name obj) tbl) (cons (symbol-value (primitive-object-lowtag obj)) (symbol-value (primitive-object-widetag obj))))))
  "Primitive objects are defined by SBCL and will not change. Convenient as a
non-unique ID prefix.")

(defparameter *simple-object-table*
  (apply 'vector '(fixnum
                   character single-float 
                   double-float bignum
                   short-float complex
                   rational string
                   pathname symbol 
                   cons hash-table 
                   standard-object struct
                   array class 
                   null t))
  "A vector containing the simple set of lisp objects.")

(defvar *core-object-table* (make-hash-table)
  "A hash-table mapping PRIMITIVE-TYPE names to integers.")

(definline prim-type (obj)
  "Return the name of the primitive type of OBJ."
  (primitive-type-name (primitive-type-of obj)))

(declaim (inline %lisp-object-id))
(defun %lisp-object-id (obj)
  "Return the STD/SERDE 'id' of OBJ - which is its position in *SIMPLE-LISP-OBJECTS*."
  (position obj *lisp-objects*))

(defmacro define-io (name &body body)
  "Define a set of readers and writers of category NAME.

BODY contains elements of the form:

(OBJECT &KEY READ WRITE)"
  (when body
    `(progn
       (defmacro ,(symbolicate 'read- name) (ty from)
         `(,(intern (string (symbolicate 'read- ',name '- ty)) ,*package*) ,from))
       (defmacro ,(symbolicate 'write- name) (ty obj to)
         `(,(intern (string (symbolicate 'write- ',name '- ty)) ,*package*) ,to ,obj))
       ,@(loop for form in body
               append 
                  (let* ((type (car form))
                         (type-name (if (consp type)
                                        (format nil "~@[~{~A-~^~A~}~]" type)
                                        type))
                         (rfn (symbolicate 'read- name '- type-name))
                         (wfn (symbolicate 'write- name '- type-name)))
                    `(,@(when-let ((rf (cdr (assoc :read (cdr form)))))
                          (when #1=(cdr rf)
                                `((defun ,rfn ,(car rf) ,@(if (atom #1#) (list #1#) #1#)))))
                      ,@(when-let ((wf (cdr (assoc :write (cdr form)))))
                          (when #2=(cdr wf)
                                `((defun ,wfn ,(car wf) ,@(if (atom #2#) (list #2#) #2#)))))))))))
