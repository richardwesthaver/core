;;; pkg.lisp

;; Q Test Packages

;;; Code:
(defpackage :q/tests/fuzz
  (:use :cl :std :rt/fuzz :q :log :obj/query))

(defpackage :q/tests
  (:use :cl :std :rt :q :log :parse/pratt :obj/query :obj/ast :obj/plan :obj/query))

