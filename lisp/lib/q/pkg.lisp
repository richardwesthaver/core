;;; q/pkg.lisp --- Q Lang Packages

;;

;;; Code:
(defpackage :q/engine
  (:use :cl :std :obj/query :obj/id)
  (:export))
           
(defpackage :q/sql
  (:use :cl :std :q/engine :parse/pratt :obj/query :obj/id)
  (:export
   :sql-tokens
   :sql-parser))

(defpackage :q/lql
  (:use :cl :std :q/engine))

;; (defpackage :q/e)

(in-package :std-user)
(defpkg :q
  (:use-reexport :q/sql :q/lql))
