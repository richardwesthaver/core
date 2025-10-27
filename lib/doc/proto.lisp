;;; doc/proto.lisp --- Doc Protocol

;; DOC Core Protocol Definitions.

;;; Commentary:

;; The documentation protocol provides a few accessors shared internally by
;; multiple definition sources (files, packages, symbols, systems), but the
;; important GFs are PRINT-DOC, PRINT-DOCUMENTATION, and DOC. DOC is an
;; accessor for the documentation of a specific class and type which wraps
;; DOCUMENTATION by default. PRINT-DOC is used to print a 'simple'
;; documentation string and PRINT-DOCUMENTATION is intended to print the
;; 'complete' documentation of a given object.

;;; Code:
(in-package :doc)

(defgeneric doc (self type))
(defgeneric (setf doc) (new self type))
(defgeneric print-doc (self &optional stream))
(defgeneric print-documentation (self &optional stream))
