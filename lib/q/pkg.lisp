;;; q/pkg.lisp --- Q Lang Packages

;;

;;; Code:
(defpackage :q/proto
  ;; (:nicknames :query)
  (:use :cl :std :ast :schema :id :srv :config :val)
  (:export 
   :query
   :query-engine
   :query-expression
   :logical-expression
   :column-expression
   :literal-expression
   :row-count
   :column-count
   :record-batch
   :make-query
   :*literal-value-types*
   :literal-value-type
   :literal-value-vector
   :projection
   :selection
   :aggregate
   :execution-context
   :physical-expression
   :scan-exec
   :scan-data
   :execute-query
   :aggregate-function
   :aggregate-function-designator
   :aggregate-expression
   :binary-expression
   :unary-expression
   :alias-expression
   :query-optimizer
   :make-physical-expression
   :query-planner
   :hash-aggregate-exec
   :filter
   :selection-exec
   :projection-exec
   :execute
   :max-physical-expression
   :aggregate-physical-expression
   :math-physical-expression
   :equiv-physical-expression
   :binary-physical-expression
   :literal-physical-expression
   :column-physical-expression
   :evaluate
   :make-record-batch
   :record-batch-p
   :copy-record-batch
   :record-batch-schema
   :record-batch-fields
   :column-size
   :column-value
   :column-type
   :column-vector
   :column-data
   :math-expression
   :add-expression
   :sub-expression
   :mult-expression
   :div-expression
   :mod-expression
   :and-expression
   :or-expression
   :lteq-expression
   :gteq-expression
   :lt-expression
   :gt-expression
   :neq-expression
   :eq-expression
   :aggregate-expression-p
   :df-proj
   :df-filter
   :df-aggregate
   :df-select
   :df-fields
   :df-data
   :limit
   :binary-expression-name
   :binary-expression-op
   :sum-expression
   :min-expression
   :max-expression
   :avg-expression
   :count-expression
   :to-field
   :column-name
   :cast-expression
   :df-exec
   :register-file
   :register-data-source
   :register-df
   :optimize-query
   :projection-pushdown-optimizer
   :extract-columns*
   :extract-columns
   :query-vop
   :logical-query-plan
   :physical-query-plan
   :query-plan
   :query-expr
   :proj
   :select
   :boolean-binary-expression
   :query-parser))

(defpackage :q/select
  (:use :cl :std :q/proto :schema :ast)
  (:shadow :select)
  (:export :select))

(defpackage :q/sql
  (:nicknames :sql)
  (:use :cl :std :q/proto :parse/pratt :id :schema :parse/proto :ast)
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
  (:use :cl :std :q/proto :id :dat/proto :ast :schema)
  (:export
   :dql-error
   :dql-data-source
   :dql-query
   :dql-expression
   :dql-variable-p
   :dql-variable
   :dql-anonymous
   :dql-anonymous-p))

(pkg:defpkg :q
  (:use :cl :std :id :schema :ast :srv)
  (:use-reexport :q/proto)
  (:export
   :*query-engine*
   :*query-dialect*
   :query-dialect-designator
   :sql :dql))

