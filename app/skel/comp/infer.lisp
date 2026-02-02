;;; infer.lisp --- Inferred Projects

;; Infer the type of a project directory or component.

;;; Commentary:

;; 

;;; Code:
(in-package :skel/comp/infer)

(defclass sk-inference-engine (engine skel) ())
(defclass sk-inferred-project (sk-project) ())

(defun sk-infer (&optional (path *default-pathname-defaults*))
  "Infer the type of a project or component given its PATH."
  (declare (ignore path)))
