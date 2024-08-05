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
;; QUERY-ENGINE may always act as a source for another engine.
(defclass query-engine (query-planner execution-context data-source)
  ((sources :initarg :sources)
   (parser :initarg :parser :type query-parser)
   (optimizer :initarg :optimizer :type query-optimizer)))
