;;; lib/doc/system.lisp --- System Documentation

;; Documentation support for a Lisp System

;;; Code:
(in-package :doc)

(defclass system-documentation ()
  ((system :initarg :system :type system :accessor doc-system)))

(defmethod name ((self system-documentation))
  (asdf:component-name (doc-system self)))

(defun system-documentation (system)
  "Return the SYSTEM-DOCUMENTATION for a specified SYSTEM."
  (let ((s (find-system system)))
    (make-instance 'system-documentation
      :system s)))

(defmethod print-object ((self system-documentation) stream)
  (with-slots (system) self
    (print-unreadable-object (self stream :type t)
      (format stream "~A" (component-name system)))))

(defmethod doc-files ((self system-documentation))
  "Return a list of source file components from SELF."
  (flet ((%rec (s) (if (typep s 'asdf:module)
                       (doc-files s)
                       (component-pathname s))))
    (flatten (mapcar #'%rec (component-children (doc-system self))))))

(defmethod doc-files ((self asdf:module))
  (flet ((%rec (s) (if (typep s 'asdf:module)
                       (doc-files s)
                       (component-pathname s))))
    (mapcar #'%rec (component-children self))))
  
;; TODO: to do this correctly we need to also check if SELF is a
;; prefix of a different system name. e.g. "DOC" and "DOC-UTILS"

;; TODO: system separator handling and optimizations
(defmethod doc-packages ((self system-documentation))
  "Return a list of packages which can be traced back to SELF. This
method will only return packages that are prefixed with the name of
SELF."
  ;; (asdf:component-loaded-p
  (let ((s (component-name (doc-system self))))
    (mapcar
     #'package-documentation
     (remove-if #'null
                (mapcar
                 (lambda (p)
                   (when (and (packagep p) 
                              (or
                               (string=
                                (string-upcase s) 
                                (package-name p))
			       (string-prefix-p 
				(concatenate 'string (string-upcase s) "-")
				(package-name p))
			       (string-prefix-p 
				(concatenate 'string (string-upcase s) "/")
				(package-name p))))
                     p))
                 (list-all-packages))))))

;; TODO 2025-03-02: handle (:feature :foo :sysname) in system-depends-on results
(defmethod doc-dependencies ((self system-documentation))
  (mapcar (lambda (x) 
             (if (consp x)
                 (if (eql (pop x) :feature)
                     (when (sb-int:featurep (pop x))
                       (system-documentation (pop x))))
                 (system-documentation x)))
          (system-depends-on (doc-system self))))

(defun find-system-dependents (system)
  "Return a list of systems which depend on SYSTEM by iterating over ASDF:REGISTER-SYSTEMS."
  (let ((r))
  (dolist (s (asdf:registered-systems))
    (setf s (find-system s))
    (when (and s (member (component-name system)
                         (mapcar
                          (lambda (dep)
                            (when (atom dep)
                              (string-downcase (format nil "~A" dep))))
                          (asdf:system-depends-on s))
                         :test #'equalp))
      (push s r)))
  r))

(defmethod doc-dependents ((self system-documentation))
  (mapcar #'system-documentation (find-system-dependents (doc-system self))))
