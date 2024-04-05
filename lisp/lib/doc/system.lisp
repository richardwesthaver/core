;;; lib/doc/system.lisp --- System Documentation

;;

;;; Code:
(in-package :doc)

(defclass system-documentation ()
  ((system :initarg :system :type asdf:system :accessor doc-system)))

(defun system-documentation (system)
  "Return the SYSTEM-DOCUMENTATION for a specified SYSTEM."
  (let ((s (asdf:find-system system)))
    (make-instance 'system-documentation
      :system s)))

(defmethod print-object ((self system-documentation) stream)
  (with-slots (system) self
    (print-unreadable-object (self stream :type t)
      (format stream "~S" (asdf:component-name system)))))

(defmethod doc-files ((self system-documentation))
  "Return a list of source file components from SELF."
  (mapcar #'asdf:component-pathname (asdf:component-children (doc-system self))))

