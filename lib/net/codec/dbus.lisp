;;; dbus.lisp --- DBUS Codec

;; DBUS Codec Definitions

;;; Code:
(in-package :net/codec/dbus)

;;; DBUS IO
;; May just want to DEFINE-IO here
#+nil
(defclass dbus-type ()
  ((name :initarg :name :reader name)
   (signature :initarg :signature)
   (sigexp-formatter :initarg :sigexp-formatter)
   (signature-parser :initarg :signature-parser)
   (alignment :initarg :alignment)
   (ser :initarg :ser :reader ser)
   (de :initarg :de :reader de)
   (checker :initarg :checker)))

#+nil
(defclass dbus-type-table ()
  ((by-name :initform (make-hash-table))
   (by-signature :initform (make-hash-table))))
