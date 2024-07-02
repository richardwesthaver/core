;;; q/pkg.lisp --- Q Lang Packages

;;

;;; Code:
(defpackage :q/engine
  (:use :cl :std :obj/query :obj/id)
  (:export))
           
(defpackage :q/sql
  (:nicknames :sql)
  (:use :cl :std :q/engine :parse/pratt :obj/query :obj/id)
  (:export
   :sql-error
   :read-sql-string
   :read-sql-stream
   :parse-expression
   :sql-tokens
   :sql-parser))

(defpackage :q/dql
  (:nicknames :dql)
  (:use :cl :std :q/engine :obj/query :obj/id :dat/sxp :dat/proto)
  (:export
   :dql-error
   :dql-data-source
   :dql-query
   :dql-expression))

;; (defpackage :q/e)

(in-package :std-user)
(defpkg :q
  (:use-reexport :q/sql :q/dql))
