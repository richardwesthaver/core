;;; infer.lisp --- Inferred Projects

;; Infer the type of a project directory based on the contents

;;; Commentary:

;; 

;;; Code:
(in-package :skel/infer)

(defclass sk-inference-engine (engine skel) ())
(defclass sk-inference-service (sk-service) ())
(defclass sk-inferred-project (sk-project) ())
