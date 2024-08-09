;;; dql.lisp --- Deductive Query Langs

;; Query Engine for Inference-based query langs.

;;; Commentary:

;; Prolog, Datalog, etc.

;;;; SQL vs Prolog

;; My current understanding is that Prolog and SQL-derived langs share much in
;; common. You can certainly do deductive logic in SQL and you can do
;; relational table-based logic in Prolog.

;; It is interesting to note that they were both discovered around the same
;; time in the 60s-70s, but in very different contexts. Prolog was intended
;; for NLP and SQL (relational-algebra/RA) was the foundation for
;; RDBMS. Prolog never really found the same sort of success had by SQL, but
;; with the AI summer in full bloom and NLP being a hot-topic, perhaps we will
;; see the scales shift.

;;;; Design

;; The WAM compiler is a bit too much to understand let alone implement at
;; this stage. The design of this package will be much simpler and optimized
;; for compatibility with Lisp Objects.

;; The design we're going for in this package is what I would consider the
;; Lisper's version of Datalog. We want to implement just enough to be useful
;; as a query language, and then use it to bootstrap a more elegant Prolog
;; compiler, likely in SYN/PROLOG.

;;;;; Data Model

;; compiled code + constants -> physical plan -> arena + hash-tables -> engine

;;;;; Compiler

;; Predicates

;; Rules/Facts

;;;;; Runtime

;; Engine 

;; Execution 

;; Persistence

;;;;; Refs

;; https://franz.com/support/documentation/11.0/prolog.html

;; https://github.com/wmannis/cl-gambol

;; https://norvig.com/paip/README.html

;; https://en.wikipedia.org/wiki/Negation_as_failure

;; https://github.com/bobschrag/clolog/blob/main/architecture.md

;;; Code:
(in-package :q/dql)

;;; Vars

(declaim (fixnum *lips*))
(defvar *lips* 0
  "Count of logical inferences performed.")

;; from GAMBOL
(defvar *interactive*           t "true iff interacting with user")
(defvar *auto-backtrack*      nil "return all solutions if true")
(defvar *last-continuation*   nil "saved state of the system")
(defvar *trail*               nil "the trail, for backtracking")
(defvar *x-env*               nil "env for goals")
(defvar *y-env*               nil "env for rules")
(defvar *top-level-envs*      nil "saves top-level environments")
(defvar *top-level-vars*      nil "saves top-level variable names")
(defvar *num-slots*            -1 "number of logical variables in a query")
(defvar *rules*  (make-hash-table) "hash table for prolog rule heads")

;;; Conditions
(define-condition dql-error (error) ())

(deferror simple-dql-error (dql-error simple-error) ())

(defun simple-dql-error (ctrl &rest args)
  (error 'simpl-dql-error :format-control ctrl :format-arguments args))

;;; CLOS
(defclass dql-query (query) ())

(defclass dql-data-source (data-source) ()
  (:documentation "Data source which can be used withing DQL expressions."))

;;; Prolog Semantics

;; NOTE 2024-08-03: we're loosely following along with CL-GAMBOL, but sticking
;; with defstructs instead of vectors for the most part. I'm willing to pay
;; the immediate cost of not vectorizing in hopes that the fact that structs
;; are vector-backed and multi-threaded contexts exist will minimize the
;; effect.

;;; Macros
(defmacro ?- (&body clauses))

(defmacro *- (head &body body))

;;; Parser
(defclass dql-parser (query-parser) ())
