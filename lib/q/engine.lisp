;;; engine.lisp --- Query Engine Protocol

;; Query Engines

;;; Commentary:

;; A QUERY-ENGINE is a single object which provides top-level interfaces for
;; all levels of Query processing.

;;; Code:
(in-package :q)

;;; Vars
(defvar *query-engine*)
(deftype query-dialect-designator () `(member :sql :dql :simple))
(declaim (query-dialect-designator *query-dialect*))
(defvar *query-dialect* :sql)

;;; Engine

;; (sb-mop:class-slots (find-class 'query-engine)) ;; service schema
(defclass query-engine (query-planner execution-context data-source engine)
  ((sources :initarg :sources)
   (parser :initarg :parser :type query-parser)
   (optimizer :initarg :optimizer :type query-optimizer)))

(defgeneric sql (self input)
  (:documentation "Process sql input and return a DATA-FRAME."))

(defgeneric dql (self input)
  (:documentation "Process dql input and return a DATA-FRAME."))

(defmethod execute ((self query-engine) (plan data-frame))
  (declare (ignore self))
  (exec plan))

(defmethod optimize-query ((self query-engine) (plan logical-query-plan))
  (optimize-query (slot-value self 'query-optimizer) plan))

(defmethod execute ((self query-engine) (plan logical-query-plan))
  (exec
   (make-physical-plan
    (optimize-query self plan))))
