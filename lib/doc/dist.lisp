;;; doc/dist.lisp --- Lisp Distribution Documentation

;; Documentation utilities for Lisp 'Distributions'. Typically this
;; refers specifically to objects of type QL-DIST:DIST.

;;; Commentary:

;; public distros: Quicklisp, Ultralisp

;;; Code:
(in-package :doc)

(defclass dist-documentation ()
  ((dist :initarg :dist :type dist :accessor doc-dist)
   (systems :initarg :systems :type list :accessor doc-systems)))

(defun dist-documentation (dist &optional all)
  "Return the DIST-DOCUMENTATION for a specified DIST."
  (unless (typep dist 'dist)
    (setf dist (find-dist (format nil "~(~A~)" dist))))
  (make-instance 'dist-documentation
    :dist dist
    :systems
    (remove-if #'null 
               (mapcar
                (lambda (s)
                  ;; may need (ignore-errors-if (error-p) body)
                  (ignore-errors
                   ;; can do better here anyway
                   (when-let ((found (find-system (doc-system s) nil)))
                     (system-documentation found))))
                (if all 
                    (provided-systems dist)
                    (installed-systems dist))))))

(defmethod print-object ((self dist-documentation) stream)
  (with-slots (dist systems) self
    (print-unreadable-object (self stream :type t)
      (format stream "~S :systems ~A" (ql-dist:name dist) (length systems)))))

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
