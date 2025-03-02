;;; doc/dist.lisp --- Lisp Distribution Documentation

;; Documentation utilities for Lisp 'Distributions'. Typically this
;; refers specifically to objects of type QL-DIST:DIST.

;;; Commentary:

;; public distros: Quicklisp, Ultralisp

;;; Code:
(in-package :doc)

(defclass dist-documentation ()
  ((dist :initarg :dist :type dist :accessor doc-dist)))

(defmethod doc-systems ((self dist-documentation))
  (provided-systems (doc-dist self)))

(defun dist-documentation (dist)
  "Return the DIST-DOCUMENTATION for a specified DIST."
  (let ((dist (if (typep dist 'dist)
                  dist
                  (find-dist (string-downcase dist)))))
    (make-instance 'dist-documentation :dist dist)))

(defmethod print-object ((self dist-documentation) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S :systems ~A" (doc-dist self) (length (doc-systems self)))))

;; maybe except an additional key for specific file types and maybe
;; include system def files..
(defmethod doc-pathnames ((self dist-documentation)) 
  "Return a list of source pathnames from SELF. Includes files and directories."
  (remove-duplicates
   (apply #'append 
          (mapcar #'doc-files
                  (doc-systems self)))))

(defmethod doc-directories ((self dist-documentation))
  "Return a list of source directories from SELF."
  (remove-if #'uiop:file-pathname-p (doc-pathnames self)))

(defmethod doc-files ((self dist-documentation))
  "Return a list of source files from SELF."
  (remove-if #'uiop:directory-pathname-p (doc-pathnames self)))
