;;; proto.lisp --- Skel Core Protocol

;; 

;;; Code:
(in-package :skel/core)

(defgeneric sk-run (self)
  (:documentation "Run the object SELF."))
(defgeneric sk-new (self &rest args &key &allow-other-keys)
  (:documentation "Create a fresh instance of object SELF."))
(defgeneric sk-call (self arg)
  (:documentation "Call SELF with ARG."))
(defgeneric sk-print (self &key &allow-other-keys)
  (:documentation "Print object SELF."))
(defgeneric sk-load (self &key &allow-other-keys)
  (:documentation "Load or reload object SELF."))
(defgeneric sk-load-component (kind form &optional path)
  (:documentation "Load a component of type KIND from provided FORM, producing an SK-COMPONENT
type. Usually calls SK-TRANSFORM or SK-CONVERT internally.

PATH is an optional directory pathname which will be merged with a filename
found in FORM. Defaults to *DEFAULT-PATHNAME-DEFAULTS*."))
(defgeneric sk-compile (self &key &allow-other-keys)
  (:documentation "Compile object SELF."))
(defgeneric sk-build (self &key &allow-other-keys)
  (:documentation "Build a skel-object."))
(defgeneric sk-convert (self)
  (:documentation "Convert the object SELF."))
(defgeneric sk-read-file (self path)
  (:documentation "Read a PATH from the filesystem with SELF."))
(defgeneric sk-write (self stream)
  (:documentation "Write object SELF to STREAM."))
(defgeneric sk-write-file (self &key path &allow-other-keys)
  (:documentation "Write from SELF to file PATH."))
(defgeneric sk-find (name self &key &allow-other-keys)
  (:documentation "Find a rule with the given NAME in SELF."))
