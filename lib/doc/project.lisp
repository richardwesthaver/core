;;; lib/doc/project.lisp --- Project Documentation

;; Document an entire project.

;;; Commentary:

;;

;;; Code:
(in-package :doc)

(defclass project-documentation (document id)
  ((project :initarg :project :accessor doc-object :type project)
   ;; TODO 2026-06-20: component-documentation
   (systems :initarg :systems :accessor doc-systems :type (vector system-documentation))))

(defun project-documentation (s &optional systems)
  "Return the documentation instance of project S."
  (unless (typep s 'project) (setf s (find-project s)))
  (make-instance 'project-documentation
    :project s
    :components systems))

(defmethod print-object ((self project-documentation) stream)
  (print-unreadable-object (self stream :type t)
    (let ((proj (slot-value self 'project)))
      (format stream "~A ~A" (name proj) (version proj)))))
    
(defmethod dependents ((self project-documentation))
  (mapcar #'system-documentation (find-system-dependents (doc-object self))))

(defmethod dependencies ((self project-documentation))
  (mapcar #'system-documentation (component-require (doc-object self))))
