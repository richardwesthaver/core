;;; math/pkg.lisp --- Core Math Packages

;;

;;; Code:
(defpackage :math/core
  (:use :cl :std))

(defpackage :math/sfc
  (:use :std-lisp :math/core)
  (:export
   #:hilbert-list
   #:hilbert-curve))

(defpackage :math/auto
  (:use :std-lisp :math/core)
  (:export :life :cellular-automata :*rule-patterns*))

(pkg:defpkg :math
  (:use :std-lisp)
  (:use-reexport :math/core :math/sfc :math/auto))
