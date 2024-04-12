;;; lib/dat/proto.lisp --- data (de)serialization

;;

;;; Code:
(in-package :dat/proto)

;;; Errors
(define-condition dat-error (std-error) ())

(define-condition serializer-error (dat-error) ())
(define-condition deserializer-error (dat-error) ())
(define-condition serde-error (dat-error) ())
;;; Serialize
(eval-always
(defvar *serializable*
  '(string simple-string octet-vector octet
    char simple-array simple-vector array
    vector)
  "List of types which can be serialized to."))

(deftype serializable-type-designator ()
  `(or (member ,@*serializable*)
       (cons (member ,@*serializable*) *)))

(defun serializable-type-p (x)
  "Return non-nil if type X is serializable."
  (typep x 'serializable-type-designator))

(defgeneric serializable-p (self)
  (:method ((self t)) nil)
  (:documentation "Return non-nil of object SELF is serializable."))

(defgeneric serialize (obj format &key)
  (:documentation "Serialize OBJ to FORMAT, which is a SERIALIZABLE-TYPE-DESIGNATOR."))

;;; Deserialize
(eval-always
(defvar *deserializable* nil
  "List of types which are DESERIALIZABLE-P"))
  
(deftype deserializable-type-designator ()
  `(or (member ,@*deserializable*)
       (cons (member ,@*deserializable*) *)))

(defun deserializable-type-p (x)
  "Return non-nil if type X is deserializable."
  (typep x 'deserializable-type-designator))

(defgeneric deserializable-p (self)
  (:method ((self t)) nil)
  (:documentation "Return non-nil if object SELF is deserializable."))

(defgeneric deserialize (from format &key)
  (:documentation "Deserialize FROM into an object of type FORMAT, which is a
DESERIALIZABLE-TYPE-DESIGNATOR."))

;;; Serde
(defgeneric serde (from to)
  (:documentation "Point-to-point serialization.

FROM and TO should both specialize on object instances.

Calling this function requires you to initialize the arguments instead
of relying on a type-designator format and generating an object in the
method body."))

