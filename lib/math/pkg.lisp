;;; math/pkg.lisp --- Core Math Packages

;;

;;; Code:
(defpackage :math-int
  (:use :cl :std)
  (:export :*math-packages*))
(in-package :math-int)
(defparameter *math-packages* nil)
(setq *defpkg-hook* (lambda (x) (pushnew (package-name x) *math-packages* :test 'string=)))

(defpkg :math/proto
  (:use :cl :std)
  (:export :math-error :math-warning))

(defpkg :math/util
  (:use :cl :std :math/proto :tensor)
  (:export :blasfunc))

(defpkg :math/sfc
  (:use :std-lisp :math/proto)
  (:export
   #:hilbert-list
   #:hilbert-curve))

(defpkg :math/auto
  (:use :std-lisp :math/proto)
  (:export :life :cellular-automata :*rule-patterns*))

(defpkg :math
  (:use :std-lisp)
  (:use-reexport :math/proto :math/sfc :math/auto))

(setq *defpkg-hook* nil)
