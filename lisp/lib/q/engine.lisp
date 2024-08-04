;;; engine.lisp --- Query Engine Protocol

;; Query Engines

;;; Commentary:

;; A QUERY-ENGINE is a single object which provides top-level interfaces for
;; all levels of Query processing.

;;; Code:
(in-package :q/proto)

(defclass query-engine (query-planner query-optimizer execution-context)
  ((data-sources)))
