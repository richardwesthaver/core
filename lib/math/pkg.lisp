;;; math/pkg.lisp --- Core Math Packages

;;

;;; Code:
(defpackage :math/proto
  (:use :cl :std)
  (:export :math-error :math-warning))

(defpackage :math/sfc
  (:use :std-lisp :math/proto)
  (:export
   #:hilbert-list
   #:hilbert-curve))

(defpackage :math/auto
  (:use :std-lisp :math/proto)
  (:export :life :cellular-automata :*rule-patterns*))

(pkg:defpkg :math
  (:use :std-lisp)
  (:use-reexport :math/proto :math/sfc :math/auto))
