;;; bench.lisp --- Core Benchmarks

;; 

;;; Code:
(in-package :std-user)

(defpkg :core/bench
  (:use :std-lisp :rt :log :rt/bench :rt/cover))

(in-package :core/bench)

(defbench simple () "")
