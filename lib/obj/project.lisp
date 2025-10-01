;;; project.lisp --- Project Protocols

;; The PROJECT base class.

;;; Commentary:

;; The SK-PROJECT class used to be 'the base class', but not all projects
;; necessarily fit into the SKEL protocol. A much more generic starting point
;; is needed.

;; The PROJECT class contains an unusually large number of slots for a
;; base. We want to fit as much of the common 'metadata' involved in projects
;; here so that we don't have to duplicate them elsewhere.

;; The PROJECT class always contains a dedicated AST slot containing an S-expr
;; representation of the project or NIL. This slot is used as a buffer when
;; reading or writing projects.

;;; Code:
(in-package :obj/project)

;;; Meta
(defclass project-metadata ()
  ((name :initarg :name :initform nil :type (or null string) :accessor name)
   (path :initarg :path :initform nil :type (or null pathname) :accessor path)
   (author :initarg :author :accessor author)
   (version :initform nil :initarg :version :accessor version)
   (tags :initform nil :initarg :tags :accessor tags)
   (description :initarg :description :initform nil :type (or null string) :accessor description)
   (license :initform nil :initarg :license :accessor license))
  (:documentation "Project Metadata contains optional slots which may be inherited by
project-like objects."))

(defclass project (id ast) ()
  (:documentation "A generic project (without metadata)."))

(defclass simple-project (project project-metadata) ()
  (:documentation "A PROJECT with optional metadata."))

(defvar *default-project-class* 'simple-project)

(defconfig project-config () ()
  (:documentation "A generic project configuration."))

(defun make-project (name &rest args &key (class *default-project-class*) &allow-other-keys)
  (apply 'make-instance class :name name (remove-from-plist args :class)))

(defmethod print-object ((self project) stream)
  (print-unreadable-object (self stream :type t)
    (princ (name self) stream)))
