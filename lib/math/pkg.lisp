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
  (:use :cl :std :tensor)
  (:export :math-error :math-warning))

(defpkg :math/util
  (:use :cl :std :math/proto :tensor)
  (:export :blasfunc :lapackfunc))

(defpkg :math/sfc
  (:use :std-lisp :math/proto)
  (:export
   #:hilbert-list
   #:hilbert-curve))

(defpkg :math/auto
  (:use :std-lisp :math/proto)
  (:export :life :cellular-automata :*rule-patterns*))

(defpkg :math/blas
  (:use :std-lisp :math/proto :blas :tensor)
  (:import-from :math/util :blasfunc)
  (:export))

(defpkg :math/syn
  (:use :std-lisp :tensor :parse/yacc)
  (:export :*linfix-parser*))

#+lapack
(defpkg :math/lapack
  (:use :std-lisp :math/proto :lapack :tensor)
  (:import-from :math/util :lapackfunc)
  (:export))

(setq *defpkg-hook* nil)
