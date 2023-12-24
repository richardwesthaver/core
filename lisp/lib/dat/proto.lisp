;;; lib/dat/proto.lisp --- data (de)serialization

;;

;;; Code:
(in-package :dat/proto)
(defgeneric serialize (obj format  &key &allow-other-keys))
(defgeneric deserialize (obj format &key &allow-other-keys))
