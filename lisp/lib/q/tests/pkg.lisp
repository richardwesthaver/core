;;; pkg.lisp

;; Q Test Packages

;;; Code:
(defpackage :q/tests/fuzz
  (:use :cl :std :rt/fuzz :q :log))

(defpackage :q/tests
  (:use :cl :std :rt :q :log :parse/pratt :obj/query))

