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

(defun system-documentation (sys &optional packages) 
  (unless (typep sys 'system) (setf sys (find-system sys)))
  (make-instance 'system-documentation 
    :system sys
    :packages (or packages 
                  (collecting
                    (mapc (lambda (x) (when (string-prefix-p (name sys) (package-name x))
                                        (collect (package-name x))))
                          (list-all-packages))))))

(defclass system-document (system-documentation org-document) ())

(defun find-system-dependents (system)
  "Return a list of systems which depend on SYSTEM."
  (when (typep system 'system) (setf system (name system)))
  (let ((r))
    (dolist (s (list-all-systems) r)
      (when (and s (member (name system)
                           (mapcar
                            (lambda (dep)
                              (when (atom dep)
                                (string-downcase (format nil "~A" dep))))
                            (component-require s))
                           :test #'equalp))
        (push s r)))))

(defmethod doc-dependents ((self system-documentation))
  (mapcar #'system-documentation (find-system-dependents (doc-system self))))

(defmethod doc-dependencies ((self system-documentation))
  (mapcar #'system-documentation (component-require (doc-system self))))

(defmethod doc-files ((self system-documentation))
  "Return a list of source file components from SELF."
  (when-let ((sys (doc-system self)))
    (cons (path sys)
          (when-let ((comp (components sys)))
            (flet ((%rec (s) (if (typep s 'mod-component)
                                 (doc-files s)
                                 (when s
                                   (path s)))))
              (flatten (mapcar #'%rec comp)))))))

(defmethod doc-files ((self mod-component))
  (flet ((%rec (s) (if (typep s 'mod-component)
                       (doc-files s)
                       (when s (path s)))))
    (mapcar #'%rec (components self))))
