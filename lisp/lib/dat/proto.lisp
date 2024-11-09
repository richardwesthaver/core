;;; lib/dat/proto.lisp --- data protocols

;; Top-level generic interface to DAT objects.

;;; Code:
(in-package :dat/proto)

;;; Errors
(define-condition dat-error (std-error) ())

(define-condition serializer-error (dat-error) ())
(define-condition deserializer-error (dat-error) ())
(define-condition serde-error (dat-error) ())
;;; Serialize
(defgeneric serializable-p (self)
  (:method ((self t)) nil)
  (:documentation "Return non-nil of object SELF is serializable."))

(defgeneric serialize (obj format &key)
  (:documentation "Serialize OBJ to FORMAT, which is a SERIALIZABLE-TYPE-DESIGNATOR."))
;;; Deserialize
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

