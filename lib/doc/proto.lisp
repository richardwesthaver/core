;;; doc/proto.lisp --- Doc Protocol

;; DOC Core Protocol Definitions.

;;; Commentary:

;; The documentation protocol provides a few accessors shared internally by
;; multiple definition sources (files, packages, symbols, systems), but the
;; important GFs are DOC, DOCUMENT, DOCUMENT-CLASS and PUBLISH. DOC is an
;; accessor for the documentation of a specific class and type which wraps
;; DOCUMENTATION by default. DOCUMENT-CLASS returns the class associated with
;; an object which is generated via the DOCUMENT method.

;;; Code:
(in-package :doc)

;;; Variables
(defvar *default-document-class* 'org-document)

(defparameter *definition-types*
  '(:variable defvar
    :constant defconstant
    :type deftype
    :symbol-macro define-symbol-macro
    :macro defmacro
    :compiler-macro define-compiler-macro
    :function defun
    :generic-function defgeneric
    :method defmethod
    :setf-expander define-setf-expander
    :structure defstruct
    :condition define-condition
    :class defclass
    :method-combination define-method-combination
    :package defpackage
    :transform :deftransform
    :optimizer :defoptimizer
    :vop :define-vop
    :source-transform :define-source-transform
    :ir1-convert :def-ir1-translator
    :declaration declaim
    :alien-type :define-alien-type)
  "Map SB-INTROSPECT definition type names to Slime-friendly forms")

;;; Protocol
(defgeneric doc (self type)
  (:documentation "Return the TYPE documentation associated with SELF. By default dispatch to
DOCUMENTATION.")
  (:method (self type) (documentation self type)))

(defgeneric (setf doc) (new self type)
  (:documentation "Set the TYPE documentation associated with SELF to NEW. By default dispatch
to (SETF DOCUMENTATION).")
  (:method (new self type) (setf (documentation self type) new)))

(defgeneric document-class (self)
  (:documentation "Return the DOCUMENT class associated with SELF or *DEFAULT-DOCUMENT-CLASS*.")
  (:method (self) 
    (declare (ignore self))
    *default-document-class*))

(defgeneric document (self &key &allow-other-keys))
;; is also SETF-able
(defgeneric (setf document) (new self))

(defgeneric publish (self &key &allow-other-keys))

;;; Utils
(defun definition-specifier (type)
  "Return a pretty specifier for NAME representing a definition of type TYPE."
  (getf *definition-types* type))

(defun make-dspec (type name source-location)
  (list* (definition-specifier type)
         name
         (sb-introspect::definition-source-description source-location)))

(defun find-definitions (name)
  "Iterate over all type definitions returning two lists, DSPECs and DEFINITION-SOURCEs."
  (let ((dspecs) (defs))
    (loop for type in *definition-types* by #'cddr
          for defsrcs = (sb-introspect:find-definition-sources-by-name name type)
          do (loop for defsrc in defsrcs
                   do (push (make-dspec type name defsrc) dspecs)
                      (dolist (d (sb-introspect:find-definition-sources-by-name name type)) (push d defs))))
    (values dspecs defs)))
