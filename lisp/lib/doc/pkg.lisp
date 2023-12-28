;;; lib/doc/pkg.lisp --- CL Documentation

;; This package is designed to help us navigate our Lisp systems,
;; packages, and files and extract information relevant to
;; documentation. This is a rather broad category. Here are some of
;; the categories of information we're interested in:

;; - Comments :: like this one.
#| or this one |#

;; - Docstrings :: typically store in symbol properties, documentation
;;   metaclass slot, etc. often found somewhere in the body of a form
;;   starting with DEF.

;; - Object Structure :: for functions - their declared type, for
;;   objects their slots, methods, sub/superclasses, etc (MOP).

;; - Source :: the source code which defines a symbol and its
;;   file/line location.

;;; Commentary:

;; Documentation is a tricky craft, good thingwe have a
;; self-documenting language :).

;;; Code:
(defpackage :doc
  (:use :cl :std :organ :sb-mop :sb-introspect)
  (:export
   :definition-specifier
   :find-definitions
   ;; err
   :doc-error
   ;; symbol
   :do-symbol* :classify-symbol :symbol-classification-string))

(in-package :doc)

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

(defun definition-specifier (type)
  "Return a pretty specifier for NAME representing a definition of type TYPE."
  (getf *definition-types* type))

(defun make-dspec (type name source-location)
  (list* (definition-specifier type)
         name
         (sb-introspect::definition-source-description source-location)))

(defun find-definitions (name)
  (loop for type in *definition-types* by #'cddr
        for defsrcs = (sb-introspect:find-definition-sources-by-name name type)
        append (loop for defsrc in defsrcs 
                     collect (list (make-dspec type name defsrc)
                                   (sb-introspect:find-definition-sources-by-name name type)))))
