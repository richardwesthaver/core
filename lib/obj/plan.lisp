;;; plan.lisp --- Planning Protocols

;; This package provides base classes for various Planning Protocols such as
;; Query Plans in OBJ/QUERY.

;;; Commentary:
;; ref: https://github.com/postgres/postgres/tree/master/src/backend/executor

#| postgres query processing

Query Processing Control Flow
-----------------------------

This is a sketch of control flow for full query processing:

	CreateQueryDesc

	ExecutorStart
		CreateExecutorState
			creates per-query context
		switch to per-query context to run ExecInitNode
		AfterTriggerBeginQuery
		ExecInitNode --- recursively scans plan tree
			ExecInitNode
				recurse into subsidiary nodes
			CreateExprContext
				creates per-tuple context
			ExecInitExpr

	ExecutorRun
		ExecProcNode --- recursively called in per-query context
			ExecEvalExpr --- called in per-tuple context
			ResetExprContext --- to free memory

	ExecutorFinish
		ExecPostprocessPlan --- run any unfinished ModifyTable nodes
		AfterTriggerEndQuery

	ExecutorEnd
		ExecEndNode --- recursively releases resources
		FreeExecutorState
			frees per-query context and child contexts

	FreeQueryDesc
|#

#| ASDF:PLAN

ASDF systems depend on a PLAN object which specifies the order of execution of
a set of actions - the default ordering is sequential. This plan is used to
build the system.

ASDF actions may refer to an associated ACTION-STATUS object which stores the
current state of an action's progress:

  ;; STAMP   KEEP-P DONE-P NEED-P     symbol bitmap  previously   currently
  ;; not-nil   T      T      T     =>  GOOD     7    up-to-date   done (e.g. file previously loaded)
  ;; not-nil   T      T     NIL    =>  HERE     6    up-to-date   unplanned yet done
  ;; not-nil   T     NIL     T     =>  REDO     5    up-to-date   planned (e.g. file to load)
  ;; not-nil   T     NIL    NIL    =>  SKIP     4    up-to-date   unplanned (e.g. file compiled)
  ;; not-nil  NIL     T      T     =>  DONE     3    out-of-date  done
  ;; not-nil  NIL     T     NIL    =>  WHAT     2    out-of-date  unplanned yet done(?)
  ;;  NIL     NIL    NIL     T     =>  TODO     1    out-of-date  planned
  ;;  NIL     NIL    NIL    NIL    =>  VOID     0    out-of-date  unplanned
  ;;
  ;; Note that a VOID status cannot happen as part of a transitive dependency of a wanted node
  ;; while traversing a node with TRAVERSE-ACTION; it can only happen while checking whether an
  ;; action is up-to-date with ACTION-UP-TO-DATE-P.
  ;;
  ;; When calling TRAVERSE-ACTION, the +need-bit+ is set,
  ;; unless the action is up-to-date and not needed-in-image (HERE, SKIP).
  ;; When PERFORMing an action, the +done-bit+ is set.
  ;; When the +need-bit+ is set but not the +done-bit+, the level slot indicates which level of
  ;; OPERATE it was last marked needed for; if it happens to be needed at a higher-level, then
  ;; its urgency (and that of its transitive dependencies) must be escalated so that it will be
  ;; done before the end of this level of operate.
  ;;
  ;; Also, when no ACTION-STATUS is associated to an action yet, NIL serves as a bottom value.
  ;;
  (defparameter +keep-bit+ 4)
  (defparameter +done-bit+ 2)
  (defparameter +need-bit+ 1)
  (defparameter +good-bits+ 7)
  (defparameter +todo-bits+ 1)
  (defparameter +void-bits+ 0)

  (defparameter +status-good+
    (make-instance 'action-status :bits +good-bits+ :stamp t))
  (defparameter +status-todo+
    (make-instance 'action-status :bits +todo-bits+ :stamp nil))
  (defparameter +status-void+
    (make-instance 'action-status :bits +void-bits+ :stamp nil)))

;;;; The four different actual traversals:
;; * TRAVERSE-ACTION o c T: Ensure all dependencies are either up-to-date in-image, or planned
;; * TRAVERSE-ACTION o c NIL: Ensure all dependencies are up-to-date or planned, in-image or not
;; * ACTION-UP-TO-DATE-P: Check whether some (defsystem-depends-on ?) dependencies are up to date
;; * COLLECT-ACTION-DEPENDENCIES: Get the dependencies (filtered), don't change any status

;;;; High-level interface: make-plan, perform-plan
|#

;;; Code:
(in-package :obj/plan)

;; RESEARCH 2024-10-27: dynamic plans
(defclass plan () ())

(defclass logical-plan (plan) ())

(defclass physical-plan (plan) ())

(defclass planner () ())

(defgeneric make-physical-plan (plan)
  (:documentation "Create a physical plan from logical plan."))
