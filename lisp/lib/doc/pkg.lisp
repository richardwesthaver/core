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
   :doc-error))

(in-package :doc)
