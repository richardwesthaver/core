;;; plan.lisp --- Execution Plans for SKEL

;; Logical/Physical Plan implementations for Skel objects

;;; Code:
(in-package :skel/core/plan)

(defclass sk-logical-plan (logical-plan skel) ()
  (:documentation "A logical plan containing SKEL objects.")
  (:default-initargs
   :id (gensym "SK-LOGICAL-PLAN")))

(defclass sk-physical-plan (physical-plan skel) ()
  (:documentation "A physical plan containing SKEL objects.")
  (:default-initargs
   :id (gensym "SK-PHYSICAL-PLAN")))

(defclass sk-planner (planner) ())
