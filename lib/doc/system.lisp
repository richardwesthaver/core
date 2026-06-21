;;; system.lisp --- Lisp System Documentation

;; Standard System Documentation.

;;; Commentary:

;; This module provides the SYSTEM-DOCUMENTATION class which wraps a
;; STD:SYSTEM and provides a basic documentation-focused API.

;; SYSTEM-DOCUMENTATION is the most high-level documentation class provided
;; and is intended to be encoded into a tree of ORG-DOCUMENT objects.

;;; Code:
(in-package :doc)

(defclass system-documentation (document id)
  ((system :initarg :system :accessor doc-system :type system)
   (packages :initarg :packages :accessor doc-packages :type (vector package-documentation))))

(defmethod print-object ((self system-documentation) stream)
  (print-unreadable-object (self stream :type t)
    (let ((sys (slot-value self 'system)))
      (format stream "~A ~A" (name sys) (version sys)))))

(defmethod description ((self system-documentation)) (description (doc-system self)))

(defun system-documentation (sys &optional packages) 
  (unless (typep sys 'system) (setf sys (find-system sys)))
  (make-instance 'system-documentation 
    :system sys
    :packages (or packages 
                  (collecting
                    (mapc (lambda (x) (when (string-prefix-p (name sys) (package-name x))
                                        (collect (package-name x))))
                          (list-all-packages))))))

(defmethod dependents ((self system-documentation))
  (mapcar #'system-documentation (find-system-dependents (doc-system self))))

(defmethod dependencies ((self system-documentation))
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
  (labels ((%rec (s) (if (typep s 'mod-component)
                       (doc-files s)
                       (when s (path s)))))
    (mapcar #'%rec (components self))))
