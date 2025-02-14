;;; engine.lisp --- Query Engine Protocol

;; Query Engines

;;; Commentary:

;; A QUERY-ENGINE is a single object which provides top-level interfaces for
;; all levels of Query processing.

;;; Code:
(in-package :q/proto)

;;; Vars
(defvar *query-engine*)
(deftype query-dialect-designator () `(member :sql :dql))
(declaim (query-dialect-designator *query-dialect*))
(defvar *query-dialect* :sql)

;;; Engine
;; NOTE 2024-08-04: only slot inherited should be :SCHEMA from DATA-SOURCE. A
;; QUERY-ENGINE may always act as a source for another engine (maybe, probably)
(defclass query-engine (query-planner execution-context data-source)
  ((sources :initarg :sources)
   (parser :initarg :parser :type query-parser)
   (optimizer :initarg :optimizer :type query-optimizer)))

(defgeneric sql (self input)
  (:documentation "Process sql input and return a DATA-FRAME."))

(defgeneric dql (self input)
  (:documentation "Process dql input and return a DATA-FRAME."))

(defmethod execute* ((self query-engine) (plan data-frame))
  (declare (ignore self))
  (execute plan))

(defmethod optimize-query ((self query-engine) (plan logical-plan))
  (optimize-query (slot-value self 'query-optimizer) plan))

(defmethod execute* ((self query-engine) (plan logical-plan))
  (execute
   (make-physical-plan
    (optimize-query self plan))))
