;;; lib/doc/pkg.lisp --- CL Documentation

;; This package is designed to help us navigate our Lisp systems,
;; packages, symbols, and files to extract information relevant to
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

;; Documentation is a tricky craft, good thing we have a
;; self-documenting language :).

;; The API consists of extractors for the above categories of
;; information and a compiler (in comp.lisp) which can be used to
;; generate output.

;; This is an SBCL library, so we use the SB-INTROSPECT project for
;; most of the heavy lifting related to inspecting Lisp objects. This
;; covers most symbols and informs the next category of extractor
;; which is the file extractor in file.lisp.

;; The package extractor in package.lisp wraps SB-INTROSPECT functions
;; and populates internal data structures with relevant information
;; based on exported symbols.

;; The file extractor is a parser which supports lisp source files as
;; well as system definitions (ASD) and possibly other inputs. You can
;; use it as a standalone document extractor for files, but it is
;; intended to be called with source-location information from
;; SB-INTROSPECT. The file extractor retrieves additional information
;; from the source file where a symbol is defined such as comments and
;; neighboring definitions.

;; The final static extractor is in system.lisp which of course is
;; intended for entire Lisp systems, combining the extractors defined
;; thus far.

;; The compiler in comp.lisp contains the high-level functions which
;; take system, package, file, or symbol names and generate
;; documentation output.

;; This library DOES NOT implement export/publishing per se. We use
;; the ORGAN system to generate ORG-DOCUMENT objects, which themselves
;; implement the functionality needed to generate *.org files and
;; translate to html,pdf,txt and other formats.

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
