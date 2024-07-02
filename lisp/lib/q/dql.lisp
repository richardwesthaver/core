;;; dql.lisp --- Deductive Query Langs

;; Query Engine for Inference-based query langs.

;;; Commentary:

;; Prolog, Datalog, etc.

;;;; Why bother with this when we have SQL?

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
;; for compatibility with Lisp.

;; I think we can get quite far 

;;; Code:
(in-package :q/dql)

;;; Conditions
(define-condition dql-error (error) ())

(deferror simple-dql-error (dql-error simple-error) ())

(defun simple-dql-error (ctrl &rest args)
  (error 'simpl-dql-error :format-control ctrl :format-arguments args))

(defclass dql-query (query) ())

(defclass dql-data-source (data-source) ()
  (:documentation "Data source which can be used withing DQL expressions."))

