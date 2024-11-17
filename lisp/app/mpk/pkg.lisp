;;; pkg.lisp --- MPK Packages

;; 

;;; Code:
(defpackage :mpk
  (:use :cl :std :log))

(defpackage :mpk/cli
  (:use :cl :std :log :mpk))
