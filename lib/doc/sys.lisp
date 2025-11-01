;;; sys.lisp --- Lisp System Documentation

;; Standard System Documentation.

;;; Commentary:

;; This module provides the SYSTEM-DOCUMENTATION class which inherits from
;; ORG-DOCUMENT, wraps a STD:SYSTEM and provides a basic documentation-focused
;; API - most often useful in producing org documentation for complete
;; systems.

;;; Code:
(in-package :doc)

(defclass system-documentation ()
  ((system :initarg :system :accessor doc-system :type std:system)
   (packages :initarg :packages :accessor doc-packages :type (vector package-documentation))))

(defmethod std/defsys::system-description ((self system-documentation)) (std/defsys::system-description (doc-system self)))

(defclass system-document (system-documentation org-document) ())
