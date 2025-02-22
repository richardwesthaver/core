;;; plan.lisp --- Execution Plans for SKEL

;; Logical/Physical Plan implementations for Skel objects

;;; Code:
(in-package :skel/core/plan)

(defclass sk-logical-plan (logical-plan skel) ()
  (:documentation "A logical plan containing SKEL objects."))

(defclass sk-physical-plan (physical-plan skel) ()
  (:documentation "A physical plan containing SKEL objects."))

(defclass sk-planner (planner ast) ()
  (:default-initargs :ast (query:make-df (make-instance 'sk-logical-plan))))
