;;; plan.lisp --- Generic Plans

;; This package provides base classes for various 'Plans' such as Query Plans
;; in OBJ/QUERY and other various Execution Contexts throughout the core.

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
;;; Code:
(in-package :obj/plan)

;; RESEARCH 2024-10-27: dynamic plans
(defclass plan () ())

(defclass planner () ())

(defgeneric plan-state (self))
(defgeneric (setf plan-state) (new-state self))
(defgeneric plan-nodes (self))
