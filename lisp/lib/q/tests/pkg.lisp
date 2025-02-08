;;; pkg.lisp

;; Q Test Packages

;;; Code:
(defpackage :q/tests/fuzz
  (:use :cl :std :rt/fuzz :q :log :plan :schema :query))

(defpackage :q/tests
  (:use :cl :std :rt :q :log :parse/pratt :query :ast :plan :schema))

