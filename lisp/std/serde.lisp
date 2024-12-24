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
  (define-condition serde-condition () ())
  (deferror serde-error (serde-condition) ()))

(deferror serializer-error (serde-error) ())
(deferror deserializer-error (serde-error) ())

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

(defgeneric ser (kind obj))
(defgeneric de (kind obj))

(defgeneric serde (from to)
  (:documentation "Point-to-point serialization.

FROM and TO should both specialize on object instances.

Calling this function requires you to initialize the arguments instead
of relying on a type-designator format and generating an object in the
method body."))

(declaim (simple-vector *lisp-objects* *simple-lisp-objects*))
(defparameter *simple-lisp-objects* 
  (apply 'vector '(fixnum
                   character single-float 
                   double-float bignum
                   short-float complex
                   rational string
                   pathname symbol 
                   cons hash-table 
                   standard-object struct
                   array class 
                   null t)))
(defvar *lisp-objects* *simple-lisp-objects*)

(defun %lisp-object-id (obj)
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
