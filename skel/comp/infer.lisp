;;; infer.lisp --- Inferred Projects

;; Infer the type of a project directory or component.

;;; Commentary:

;; 

;;; Code:
(in-package :skel/comp/infer)

(defclass project-inference-engine (engine) ())
(defclass inferred-project (project) ())

(defun project-infer (&optional (path *default-pathname-defaults*))
  "Infer the type of a project or component given its PATH."
  (declare (ignore path)))
