;;; lib/dat/proto.lisp --- data (de)serialization

;;

;;; Code:
(in-package :dat/proto)

;;; Errors
(define-condition dat-error (std-error) ())

(define-condition serializer-error (dat-error) ())
(define-condition deserializer-error (dat-error) ())

(defgeneric serialize (obj format  &key &allow-other-keys))
(defgeneric deserialize (obj format &key &allow-other-keys))
