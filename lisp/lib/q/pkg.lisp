;;; q/pkg.lisp --- Q Lang Packages

;;

;;; Code:
(defpackage :q/proto
  (:use :cl :std :query :plan :id :schema :ast)
  (:export
   :query-engine :query-parser
   :*query-engine*
   :*query-dialect*
   :query-dialect-designator
   :sql :dql))

(defpackage :q/simple
  (:use :cl :std :q/proto :obj/query :obj/plan :obj/schema :obj/ast))

(defpackage :q/sql
  (:nicknames :sql)
  (:use :cl :std :q/proto :parse/pratt :query :id :schema :parse/proto :ast :plan)
  (:export
   :sql-error
   :read-sql-string
   :read-sql-stream
   :parse-expression
   :sql-tokens
   :sql-parser
   :simple-sql-error
   :sql-token-error
   :illegal-sql-state
   :sql-query
   :sql-data-source
   :sql-expression
   :sql-expression-vector
   :sql-identifier
   :sql-string
   :sql-number
   :sql-function
   :sql-alias
   :sql-cast
   :sql-sort
   :sql-relation
   :sql-select
   :sql-planner
   :sql-optimizer
   :*sql-token-types*
   :sql-token-type-designator
   :*sql-keywords*
   :*sql-keyword-table*
   :*sql-symbol-table*
   :get-sql-keyword
   :get-sql-symbol
   :*sql-symbols*
   :sql-token
   :next-sql-token
   :with-sql-parser
   :sql-math-expression
   :sql-binary-expression
   :with-sql-stream
   :with-sql-string
   :with-sql
   :make-sql-df))

(defpackage :q/dql
  (:nicknames :dql)
  (:use :cl :std :q/proto :query :id :dat/sxp :dat/proto :ast :schema :plan)
  (:export
   :dql-error
   :dql-data-source
   :dql-query
   :dql-expression
   :dql-variable-p
   :dql-variable
   :dql-anonymous
   :dql-anonymous-p))

;; (defpackage :q/e)

(in-package :std-user)
(defpkg :q
  (:use-reexport :q/proto :q/sql :q/dql))
