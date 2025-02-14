;;; infer.lisp --- Inferred Projects

;; Infer the structure and contents of a non-Skel project

;;; Commentary:

;; 

;;; Code:
(in-package :skel/infer)

(defclass sk-inference-engine (engine skel) ())
(defclass sk-inference-service (sk-service) ())

(defclass sk-inferred-project (sk-project) ())
