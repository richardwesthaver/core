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
;;   objects their slots, methods, sub/superclasses, allocation info,
;;   etc.

;; - Source :: the source code which defines a symbol and its
;;   file/line location.

;;; Commentary:

;; Documentation is a tricky craft, good thing we have a
;; self-documenting language :).

;; The API consists of extractors for the above categories of
;; information and a compiler (in comp.lisp) which can be used to
;; generate documentation output.

;; This library DOES NOT implement export/publishing per se. We use
;; the ORGAN system to generate ORG-DOCUMENT objects, which themselves
;; implement the functionality needed to generate *.org files and
;; translate to html,pdf,txt and other formats.

;;; Code:
(eval-when (:compile-toplevel :load-toplevel) (require :sb-introspect))

(defpackage :doc
  (:use :cl :std :organ :sb-mop :sb-introspect :obj/id :log)
  (:import-from :uiop :string-prefix-p)
  (:shadowing-import-from :asdf :component-name :component-children
   :system :component-pathname :find-system :system-description
   :system-depends-on)
  (:import-from :sb-c :packed-info :symbol-hash :symbol-dbinfo :vop-p :package-external-symbol-count)
  (:import-from :sb-kernel :symbol-package-id)
  (:import-from :sb-ext :restrict-compiler-policy)
  (:import-from :ql-dist :dist :find-dist :provided-systems :installed-systems)
  (:import-from :sb-impl :print-standard-describe-header :describe-object)
  (:import-from :sb-int :condition)
  (:import-from :sb-alien :alien-type-p)
  (:export
   :definition-specifier
   :find-definitions
   ;; methods
   :doc-file :doc-files :doc-symbol :doc-symbols :doc-package :doc-packages :doc-dist
   :doc-pathnames :doc-directories :doc-parse :doc-system :doc-dependencies :doc-dependents
   ;; symbol
   :do-symbol* :classify-symbol :symbol-classification-string
   :symbol-documentation
   ;; package
   :package-documentation
   ;; file
   :define-source-file* :*source-file-types*
   :file-heading :file-headline :file-header :read-file-header
   :+max-heading-level+ :+min-heading-level+
   :file-documentation
   ;; system
   :system-documentation
   ;; dist
   :dist-documentation
   ;; image
   :image-documentation))

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
  "Iterate over all type definitions returning two lists, DSPECs and DEFINITION-SOURCEs."
  (let ((dspecs) (defs))
    (loop for type in *definition-types* by #'cddr
          for defsrcs = (sb-introspect:find-definition-sources-by-name name type)
          do (loop for defsrc in defsrcs
                   do (push (make-dspec type name defsrc) dspecs)
                      (dolist (d (sb-introspect:find-definition-sources-by-name name type)) (push d defs))))
    (values defs dspecs)))

;;; DEFSYS Providers
;; (defprovider :doc (name &rest args))
